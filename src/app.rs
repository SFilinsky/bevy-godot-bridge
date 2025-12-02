use bevy::app::{App};
use godot::{
    classes::{INode, Node},
    obj::{Base},
    prelude::{GodotClass, godot_api},
};

use crate::prelude::*;
use std::{
    panic::{AssertUnwindSafe, catch_unwind, resume_unwind},
    sync::Mutex,
};
use bevy::DefaultPlugins;
use bevy::prelude::{Fixed, Time, Virtual};
use bevy::time::TimeUpdateStrategy;
use godot::obj::Singleton;
use crate::app_action_queue::{ActionQueue};

lazy_static::lazy_static! {
    #[doc(hidden)]
    pub static ref APP_BUILDER_FN: Mutex<Option<Box<dyn Fn(&mut App) + Send>>> = Mutex::new(None);
}

#[derive(Default, bevy::prelude::Resource)]
struct GodotClock {
    last_usec: Option<u64>,
}

#[derive(GodotClass, Default)]
#[class(base=Node)]
pub struct BevyApp {
    app: Option<App>,

    pub action_queue: ActionQueue
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
        app.insert_resource(TimeUpdateStrategy::ManualDuration(std::time::Duration::ZERO));
        app.insert_resource(Time::<Fixed>::from_hz(60.0));


        let mut virtual_time  = app.world_mut()
            .resource_mut::<Time<Virtual>>();

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
