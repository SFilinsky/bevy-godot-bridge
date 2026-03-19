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

pub mod prelude {
    pub use super::app_action_queue::AppAction;
    pub use super::debug::cuboids::subsystem::{CuboidConfig, DebugCuboidSubsystem};
    pub use super::debug::debug_manager::{DebugRenderGateSubsystem, EDebugState};
    pub use super::debug::heatmap::subsystem::{DebugHeatmapSubsystem, HeatmapConfig};
    pub use super::debug::paths::subsystem::{DebugPathSubsystem, PathConfig};
    pub use super::debug::plugins::DebugVisualizationPlugin;
    pub use super::erased_gd::{ErasedGd, ErasedGdResource};
    pub use super::scene::GodotScene;
    pub use super::scene_tree::SceneTreeSubsystem as SceneTreeRef;
    pub use super::scene_tree::SceneTreeSubsystem;
    pub use super::utils::{
        AsPhysicsSystem, AsVisualSystem, GodotPhysicsFrame, GodotVisualFrame,
        SystemDeltaTimerSubsystem,
    };
    pub use crate::app::{BevyApp, BevyAppSubsystem};
    pub use crate::dto::{BuildsDto, DataTransferConfig, WithGatherer};
    pub use crate::import::{
        importers::EntityImporter,
        plugins::EntityInitializationPlugin,
        position::plugins::PositionInitializationPlugin,
        sets::PostEntityInitSet,
        subsystems::{IdentityRegistry, IdentitySubsystem},
    };
    pub use crate::tools::collect_children;
    pub use bevy_godot4_proc_macros::{
        ExportComponent, ExportComponentNew, ExportEntity, bevy_app, export_bundle, import_bundle,
        import_queue,
    };
}
pub use crate::app::APP_BUILDER_FN;
