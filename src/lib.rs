pub extern crate self as bevy_godot4;
pub use godot;

mod app;
mod app_action_queue;
#[cfg(feature = "assets")]
mod assets;
mod debug;
mod dto;
mod erased_gd;
mod import;
mod performance;
mod scene;
mod scene_tree;
mod tools;
mod utils;

pub use dto::DataTransferConfig;
pub use tools::collect_children;

pub mod prelude {
    pub use super::app_action_queue::AppAction;
    pub use super::debug::cuboids::resources::{CuboidConfig, DebugCuboidRequests};
    pub use super::debug::heatmap::resources::{DebugHeatmapRequests, HeatmapConfig};
    pub use super::debug::lines::resources::{DebugLineRequests, LineConfig};
    pub use super::debug::plugins::DebugVisualizationPlugin;
    pub use super::erased_gd::{ErasedGd, ErasedGdResource};
    pub use super::scene::GodotScene;
    pub use super::scene_tree::SceneTreeRef;
    pub use super::utils::{
        AsPhysicsSystem, AsVisualSystem, GodotPhysicsFrame, GodotVisualFrame, SystemDeltaTimer,
    };
    pub use crate::import::{
        components::GodotEntity, importers::EntityImporter, plugins::EntityInitializationPlugin,
        position::plugins::PositionInitializationPlugin, sets::PostEntityInitSet,
    };
    pub use crate::{DataTransferConfig, collect_children};
    pub use bevy_godot4_proc_macros::{
        ExportComponent, ExportComponentNew, ExportEntity, bevy_app, export_bundle, import_queue,
    };
}
pub use app::{APP_BUILDER_FN, BevyApp};
