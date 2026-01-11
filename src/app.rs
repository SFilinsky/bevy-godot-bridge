use bevy::app::App;
use godot::{
    classes::{INode, Node},
    obj::Base,
    prelude::{GodotClass, godot_api},
};

use crate::app_action_queue::ActionQueue;
use crate::performance::init_performance_tracing;
use crate::prelude::*;
use bevy::DefaultPlugins;
use bevy::prelude::{Fixed, Time, Virtual, World};
use bevy::time::TimeUpdateStrategy;
use godot::obj::Singleton;
use godot::prelude::{Gd, SceneTree};
use std::sync::atomic::{AtomicU64, Ordering};
use std::{
    panic::{AssertUnwindSafe, catch_unwind, resume_unwind},
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
    NoSceneTree,
    NoRoot,
    SingletonMissing,
    WrongType,
}

impl std::fmt::Display for BevyAppLookupError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BevyAppLookupError::NoSceneTree => write!(f, "SceneTree not available"),
            BevyAppLookupError::NoRoot => write!(f, "Scene root not available"),
            BevyAppLookupError::SingletonMissing => {
                write!(f, "BevyAppSingleton not found at /root")
            }
            BevyAppLookupError::WrongType => write!(f, "Node 'BevyAppSingleton' is not a BevyApp"),
        }
    }
}

#[derive(GodotClass, Default)]
#[class(base=Node)]
pub struct BevyApp {
    app: Option<App>,

    pub action_queue: ActionQueue,

    next_entity_id: AtomicU64,
}

impl BevyApp {
    pub fn get_app(&self) -> Option<&App> {
        self.app.as_ref()
    }

    pub fn get_app_mut(&mut self) -> Option<&mut App> {
        self.app.as_mut()
    }
}

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

    pub fn find_for(host: &Gd<Node>) -> Result<Gd<Self>, BevyAppLookupError> {
        // 1) parent chain (your existing method)
        if let Ok(app) = Self::find_in_parents(host) {
            return Ok(app);
        }

        // 2) singleton at /root
        let tree = host.get_tree().ok_or(BevyAppLookupError::NoSceneTree)?;
        Self::find_singleton(&tree)
    }

    /// Find BevyApp by scanning `/root/BevyAppSingleton`.
    ///
    /// This is the recommended strategy for nodes that are not children of BevyApp.
    fn find_singleton(tree: &Gd<SceneTree>) -> Result<Gd<Self>, BevyAppLookupError> {
        let root = tree.get_root().ok_or(BevyAppLookupError::NoRoot)?;

        let Some(node) = root.get_node_or_null("BevyAppSingleton") else {
            return Err(BevyAppLookupError::SingletonMissing);
        };

        node.try_cast::<BevyApp>()
            .map_err(|_| BevyAppLookupError::WrongType)
    }

    /// Find the nearest BevyApp in the parent chain starting at `start`.
    ///
    /// This is multi-instance safe: the "current instance" is determined by scene tree locality.
    fn find_in_parents(start: &Gd<Node>) -> Result<Gd<BevyApp>, String> {
        let mut cur: Option<Gd<Node>> = Some(start.clone());

        while let Some(node) = cur {
            if let Ok(app) = node.clone().try_cast::<BevyApp>() {
                return Ok(app);
            }
            cur = node.get_parent();
        }

        Err("No BevyApp found in parent chain. Importer must be under a BevyApp node.".to_string())
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

    pub fn alloc_entity_id(&mut self) -> u64 {
        self.next_entity_id.fetch_add(1, Ordering::Relaxed)
    }
}

#[godot_api]
impl INode for BevyApp {
    fn init(_base: Base<Node>) -> Self {
        Default::default()
    }

    fn ready(&mut self) {
        if godot::classes::Engine::singleton().is_editor_hint() {
            return;
        }

        init_performance_tracing();

        let mut app = App::new();
        (APP_BUILDER_FN.lock().unwrap().as_mut().unwrap())(&mut app);

        app.add_plugins(DefaultPlugins)
            .add_plugins(crate::scene::PackedScenePlugin)
            .init_non_send_resource::<crate::scene_tree::SceneTreeRefImpl>();
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
