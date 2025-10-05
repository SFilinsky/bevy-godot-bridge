use bevy::app::App;
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
use crate::app_action_queue::{ActionQueue};

lazy_static::lazy_static! {
    #[doc(hidden)]
    pub static ref APP_BUILDER_FN: Mutex<Option<Box<dyn Fn(&mut App) + Send>>> = Mutex::new(None);
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
        app.add_plugins(bevy::app::TaskPoolPlugin::default())
            .add_plugins(bevy::log::LogPlugin::default())
            .add_plugins(bevy::diagnostic::FrameCountPlugin)
            .add_plugins(bevy::diagnostic::DiagnosticsPlugin)
            .add_plugins(bevy::time::TimePlugin)
            .add_plugins(crate::scene::PackedScenePlugin)
            .init_non_send_resource::<crate::scene_tree::SceneTreeRefImpl>();
        // .add_plugins(GodotSignalsPlugin)
        // .add_plugins(GodotInputEventPlugin);

        #[cfg(feature = "assets")]
        app.add_plugins(crate::assets::GodotAssetsPlugin);

        self.app = Some(app);

        self.apply_pending_actions();
    }

    fn process(&mut self, _delta: f64) {
        if godot::classes::Engine::singleton().is_editor_hint() {
            return;
        }

        self.apply_pending_actions();

        if let Some(app) = self.app.as_mut() {

            app.insert_resource(GodotVisualFrame);

            if let Err(e) = catch_unwind(AssertUnwindSafe(|| app.update())) {
                self.app = None;

                eprintln!("bevy app update panicked");
                resume_unwind(e);
            }

            app.world_mut().remove_resource::<GodotVisualFrame>();
        }
    }

    fn physics_process(&mut self, _delta: f64) {
        if godot::classes::Engine::singleton().is_editor_hint() {
            return;
        }

        self.apply_pending_actions();

        if let Some(app) = self.app.as_mut() {
            app.insert_resource(GodotPhysicsFrame);

            if let Err(e) = catch_unwind(AssertUnwindSafe(|| app.update())) {
                self.app = None;

                eprintln!("bevy app update panicked");
                resume_unwind(e);
            }

            app.world_mut().remove_resource::<GodotPhysicsFrame>();
        }
    }
}
