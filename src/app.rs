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

    base: Base<Node>,
}

impl BevyApp {
    pub fn get_app(&self) -> Option<&App> {
        self.app.as_ref()
    }

    pub fn get_app_mut(&mut self) -> Option<&mut App> {
        self.app.as_mut()
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
        app.insert_resource(Time::<Fixed>::from_hz(60.0));

        let mut virtual_time = app.world_mut().resource_mut::<Time<Virtual>>();

        // Cap virtual time jumps to avoid huge dt after long stalls.
        // ~20 FPS cap for big spikes
        virtual_time.set_max_delta(std::time::Duration::from_millis(50));
    }

    fn update_clock(app: &mut App) {
        let world = app.world_mut();
        let now_us = godot::classes::Time::singleton().get_ticks_usec();

        let mut clock = world.resource_mut::<GodotClock>();
        let dt = if let Some(prev) = clock.last_usec {
            let du = (now_us - prev).max(0);
            std::time::Duration::from_micros(du)
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

#[godot_api]
impl INode for BevyApp {
    fn init(base: Base<Self::Base>) -> Self {
        Self {
            app: None,
            node_host: NodePath::default(),
            action_queue: ActionQueue::default(),
            next_entity_id: Arc::new(AtomicI64::new(1)),
            performance_scope_id: allocate_app_scope_id(),
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
    }

    fn process(&mut self, _delta_seconds: f64) {
        if godot::classes::Engine::singleton().is_editor_hint() {
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
