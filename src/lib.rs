pub extern crate self as bevy_godot4;
pub use godot;

mod app;
#[cfg(feature = "assets")]
mod assets;
mod erased_gd;
mod scene;
mod scene_tree;
mod utils;
mod app_action_queue;
mod tools;
mod dto;

pub use tools::collect_children;
pub use dto::ExportMeta;

pub mod prelude {
    pub use super::erased_gd::{ErasedGd, ErasedGdResource};
    pub use super::scene::GodotScene;
    pub use super::scene_tree::SceneTreeRef;
    pub use super::utils::{
        AsPhysicsSystem, AsVisualSystem, GodotPhysicsFrame, GodotVisualFrame, SystemDeltaTimer,
    };
    pub use bevy_godot4_proc_macros::{
        bevy_app,
        ExportComponent,
        ExportEntity
    };
    pub use super::app_action_queue::{AppAction};
    pub use crate::{collect_children, ExportMeta};
}
pub use app::{BevyApp, APP_BUILDER_FN};
