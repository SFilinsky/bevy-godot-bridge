pub use crate::bevy_app_subsystem::BevyAppSubsystem;
use crate::bevy_app_subsystem::{BevyAppHostPlugin, BevyAppIdAllocatorRef};
use bevy::app::App;
use godot::{
    builtin::NodePath,
    classes::{INode, Node},
    obj::{Base, WithBaseField},
    prelude::{godot_api, GodotClass, Inherits},
};

use crate::app_action_queue::ActionQueue;
use crate::import::plugins::IdentitySubsystemPlugin;
use crate::performance::init_performance_tracing;
use crate::performance::layer::{allocate_app_scope_id, enter_app_scope};
use crate::prelude::*;
use crate::scene_tree::plugins::SceneTreeSubsystemPlugin;
use bevy::prelude::{Fixed, Time, Virtual, World};
use bevy::time::TimeUpdateStrategy;
use bevy::DefaultPlugins;
use bevy_godot4::scene::PackedScenePlugin;
use godot::global::godot_error;
use godot::obj::Singleton;
use godot::prelude::Gd;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;
use std::{
    panic::{catch_unwind, resume_unwind, AssertUnwindSafe},
    sync::Mutex,
};

const VIRTUAL_TIME_MAX_DELTA_MS: f64 = 50.0;
const BEVY_FIXED_TIMESTEP_HZ: f64 = 8.0;

lazy_static::lazy_static! {
    #[doc(hidden)]
    pub static ref APP_BUILDER_FN: Mutex<Option<Box<dyn Fn(&mut App) + Send>>> = Mutex::new(None);
}

#[derive(Default, bevy::prelude::Resource)]
struct GodotClock {
    last_usec: Option<u64>,
}

#[derive(Debug)]
pub enum BevyAppLookupError {
    MissingInParentChain,
    MissingSceneRoot,
    MissingUnderSceneRoot,
    MultipleUnderSceneRoot,
}

impl std::fmt::Display for BevyAppLookupError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BevyAppLookupError::MissingInParentChain => {
                write!(f, "No BevyApp found in host parent chain")
            }
            BevyAppLookupError::MissingSceneRoot => {
                write!(f, "No SceneRoot found in host parent chain")
            }
            BevyAppLookupError::MissingUnderSceneRoot => {
                write!(f, "No BevyApp found as direct child of resolved SceneRoot")
            }
            BevyAppLookupError::MultipleUnderSceneRoot => {
                write!(f, "Multiple BevyApp nodes found under resolved SceneRoot")
            }
        }
    }
}

#[derive(GodotClass)]
#[class(base=Node)]
pub struct BevyApp {
    app: Option<App>,

    #[export]
    node_host: NodePath,

    pub action_queue: ActionQueue,

    next_entity_id: Arc<AtomicI64>,
    performance_scope_id: u64,
    // Scene-authored import nodes must enqueue their startup data before Bevy's
    // first update. This gate is released by InitializationCoordinator after it
    // calls all registered Godot initializers.
    // This is intentionally owned by BevyApp because BevyApp owns app.update().
    // The coordinator may mark readiness, but it must not drive the Bevy loop.
    scene_initialized: bool,
    // The coordinator releases the gate from Godot process. Waiting one more
    // BevyApp process lets deferred Godot listeners subscribe before startup
    // state transitions are emitted.
    startup_process_delay_remaining: u8,

    base: Base<Node>,
}

impl BevyApp {
    pub fn get_app(&self) -> Option<&App> {
        self.app.as_ref()
    }

    pub fn get_app_mut(&mut self) -> Option<&mut App> {
        self.app.as_mut()
    }

    /// Release the startup gate after Godot scene initializers submitted their
    /// import data. The next BevyApp::process() call will drain queued imports
    /// and run the first Bevy update.
    pub fn mark_scene_initialized(&mut self) {
        self.scene_initialized = true;
    }

    /// Release the startup gate after one more Godot process callback.
    ///
    /// InitializationCoordinator calls this from its own process callback. The
    /// delay gives deferred Godot listeners a chance to connect before Bevy
    /// emits startup state transitions.
    pub fn mark_scene_initialized_after_process_delay(&mut self) {
        self.mark_scene_initialized();
        self.startup_process_delay_remaining = 1;
    }
}

#[godot_api]
impl BevyApp {
    fn apply_pending_actions(&mut self) {
        if let Some(app) = self.app.as_mut() {
            for a in self.action_queue.drain() {
                a.apply(app);
            }
        }
    }

    fn configure_manual_time(app: &mut App) {
        app.init_resource::<GodotClock>();
        // We will feed Real time ourselves before every `app.update()`.
        app.insert_resource(TimeUpdateStrategy::ManualDuration(
            std::time::Duration::ZERO,
        ));
        app.insert_resource(Time::<Fixed>::from_hz(BEVY_FIXED_TIMESTEP_HZ));

        let mut virtual_time = app.world_mut().resource_mut::<Time<Virtual>>();

        // Cap virtual time jumps to avoid huge dt after long stalls.
        // ~20 FPS cap for big spikes
        virtual_time.set_max_delta(std::time::Duration::from_millis(
            VIRTUAL_TIME_MAX_DELTA_MS as u64,
        ));
    }

    fn update_clock(app: &mut App) {
        let world = app.world_mut();
        let now_us = godot::classes::Time::singleton().get_ticks_usec();
        let time_scale = scaled_godot_time_scale();
        let mut virtual_time = world.resource_mut::<Time<Virtual>>();
        virtual_time.set_max_delta(std::time::Duration::from_secs_f64(
            VIRTUAL_TIME_MAX_DELTA_MS / 1_000.0 * time_scale.max(1.0),
        ));

        let mut clock = world.resource_mut::<GodotClock>();
        let dt = if let Some(prev) = clock.last_usec {
            let du = (now_us - prev).max(0);
            std::time::Duration::from_micros(du).mul_f64(time_scale)
        } else {
            std::time::Duration::ZERO
        };
        clock.last_usec = Some(now_us);

        *world.resource_mut::<TimeUpdateStrategy>() = TimeUpdateStrategy::ManualDuration(dt);
    }

    fn resolve_via_scene_root(host: &Gd<Node>) -> Result<Gd<BevyApp>, BevyAppLookupError> {
        let Some(scene_root) = SceneRoot::resolve_as_parent(host) else {
            return Err(BevyAppLookupError::MissingSceneRoot);
        };

        let mut apps = collect_children::<BevyApp>(scene_root.upcast::<Node>(), false);

        if apps.is_empty() {
            return Err(BevyAppLookupError::MissingUnderSceneRoot);
        }

        if apps.len() > 1 {
            return Err(BevyAppLookupError::MultipleUnderSceneRoot);
        }

        Ok(apps.remove(0))
    }

    fn resolve_as_parent(start: &Gd<Node>) -> Result<Gd<BevyApp>, BevyAppLookupError> {
        let mut cur: Option<Gd<Node>> = Some(start.clone());

        while let Some(node) = cur {
            if let Ok(app) = node.clone().try_cast::<BevyApp>() {
                return Ok(app);
            }
            cur = node.get_parent();
        }

        Err(BevyAppLookupError::MissingInParentChain)
    }

    pub fn resolve<T>(host: &Gd<T>) -> Result<Gd<Self>, BevyAppLookupError>
    where
        T: GodotClass + Inherits<Node>,
    {
        let host_node: Gd<Node> = host.clone().upcast();

        let result = match Self::resolve_as_parent(&host_node) {
            Ok(app) => Ok(app),
            Err(_) => Self::resolve_via_scene_root(&host_node),
        };

        if let Err(err) = &result {
            let host_path = if host_node.is_inside_tree() {
                host_node.get_path().to_string()
            } else {
                "<detached host>".to_string()
            };

            godot_error!(
                "BevyApp::resolve() failed for host '{}': {}",
                host_path,
                err
            );
        }

        result
    }

    /// Run a closure with a mutable reference to this instance's Bevy World.
    pub fn with_world_mut<F>(&mut self, f: F)
    where
        F: FnOnce(&mut World),
    {
        let app = self
            .get_app_mut()
            .expect("BevyApp is not initialized: get_app_mut() returned None in with_world_mut");
        let world = app.world_mut();
        f(world);
    }

    pub fn alloc_entity_id(&mut self) -> i64 {
        self.next_entity_id.fetch_add(1, Ordering::Relaxed)
    }

    pub fn performance_scope_id(&self) -> u64 {
        self.performance_scope_id
    }

    pub fn resolve_node_host(&self) -> Gd<Node> {
        if self.node_host.is_empty() {
            return self.base().clone().upcast();
        }

        let Some(node) = self.base().get_node_or_null(&self.node_host) else {
            return self.base().clone().upcast();
        };

        match node.try_cast::<Node>() {
            Ok(node) => node,
            Err(_) => self.base().clone().upcast(),
        }
    }

}

fn scaled_godot_time_scale() -> f64 {
    let mut engine = godot::classes::Engine::singleton();
    let time_scale = engine.get_time_scale();
    if time_scale.is_finite() && time_scale >= 0.0 {
        time_scale
    } else {
        0.0
    }
}

#[godot_api]
impl INode for BevyApp {
    fn init(base: Base<Self::Base>) -> Self {
        Self {
            app: None,
            node_host: NodePath::default(),
            action_queue: ActionQueue::default(),
            next_entity_id: Arc::new(AtomicI64::new(1)),
            performance_scope_id: allocate_app_scope_id(),
            scene_initialized: false,
            startup_process_delay_remaining: 0,
            base,
        }
    }
    fn enter_tree(&mut self) {
        if godot::classes::Engine::singleton().is_editor_hint() {
            return;
        }

        init_performance_tracing();

        let mut app = App::new();

        app.add_plugins(DefaultPlugins)
            .add_plugins(PackedScenePlugin)
            .add_plugins(SceneTreeSubsystemPlugin)
            .add_plugins(IdentitySubsystemPlugin)
            .add_plugins(BevyAppHostPlugin::with_host(
                self.base().clone().upcast(),
                self.resolve_node_host(),
            ))
            .insert_non_send_resource(BevyAppIdAllocatorRef::new(self.next_entity_id.clone()));

        (APP_BUILDER_FN.lock().unwrap().as_mut().unwrap())(&mut app);

        // .add_plugins(GodotSignalsPlugin)
        // .add_plugins(GodotInputEventPlugin);

        Self::configure_manual_time(&mut app);

        #[cfg(feature = "assets")]
        app.add_plugins(crate::assets::GodotAssetsPlugin);

        self.app = Some(app);

        self.apply_pending_actions();

        // Scenes without an InitializationCoordinator keep the old standalone
        // behavior: Bevy starts ticking as soon as BevyApp is ready.
        if !InitializationCoordinator::exists_for(&self.base().clone().upcast::<Node>()) {
            self.mark_scene_initialized();
        }
    }

    fn process(&mut self, _delta_seconds: f64) {
        if godot::classes::Engine::singleton().is_editor_hint() {
            return;
        }

        // Parent nodes process before children in Godot. Without this guard,
        // BevyApp can tick before InitializationCoordinator has called scene
        // initializers, so gameplay systems can observe an empty startup world.
        if !self.scene_initialized {
            return;
        }
        if self.startup_process_delay_remaining > 0 {
            self.startup_process_delay_remaining -= 1;
            return;
        }

        self.apply_pending_actions();

        if let Some(app) = self.app.as_mut() {
            Self::update_clock(app);

            app.insert_resource(GodotVisualFrame);
            let _scope_guard = enter_app_scope(self.performance_scope_id);
            if let Err(e) = catch_unwind(AssertUnwindSafe(|| app.update())) {
                self.app = None;

                eprintln!("bevy app update panicked");
                resume_unwind(e);
            }
            app.world_mut().remove_resource::<GodotVisualFrame>();
        }
    }

    fn physics_process(&mut self, _delta_seconds: f64) {
        // if godot::classes::Engine::singleton().is_editor_hint() {
        //     return;
        // }
        //
        // self.apply_pending_actions();
        //
        // if let Some(app) = self.app.as_mut() {
        //
        //     Self::update_clock(app);
        //
        //     app.insert_resource(GodotPhysicsFrame);
        //     if let Err(e) = catch_unwind(AssertUnwindSafe(|| app.update())) {
        //         self.app = None;
        //
        //         eprintln!("bevy app update panicked");
        //         resume_unwind(e);
        //     }
        //     app.world_mut().remove_resource::<GodotPhysicsFrame>();
        // }
    }
}
