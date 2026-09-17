//! Tools for using Bevy inside a Godot scene.
//!
//! The bridge helps Godot and Bevy share data without either side reaching into
//! the other. It can import data from Godot, show Bevy state in Godot, and run
//! actions started by Godot. See the README first when you are new to the
//! library.

pub extern crate self as bevy_godot4;
pub use godot;

/// Types used by actions that Godot starts and Bevy performs.
pub mod action_framework;
mod app;
mod app_action_queue;
mod bevy_app_subsystem;
#[cfg(feature = "assets")]
mod assets;
mod debug;
mod dto;
mod entity_meta;
mod entity_registry;
mod erased_gd;
mod import;
mod initialization;
/// Queues Bevy values until Godot reads a batch.
pub mod export_queue;
mod performance;
mod required_settings;
mod scene;
mod scene_tree;
mod state;
mod tools;
mod utils;

/// The normal imports for game code using this bridge.
///
/// Start with this module instead of importing private bridge files. Its macros
/// and traits are the supported ways to extend the bridge.
pub mod prelude {
    pub use super::app_action_queue::AppAction;
    pub use super::debug::cuboids::subsystem::{CuboidConfig, DebugCuboidSubsystem};
    pub use super::debug::debug_manager::{DebugRenderGateSubsystem, EDebugState};
    pub use super::debug::heatmap::subsystem::{DebugHeatmapSubsystem, HeatmapConfig};
    pub use super::debug::paths::subsystem::{DebugPathSubsystem, PathConfig};
    pub use super::debug::plugins::DebugVisualizationPlugin;
    pub use super::entity_meta::EntityMeta;
    pub use super::entity_registry::EntityRegistry;
    pub use super::erased_gd::{ErasedGd, ErasedGdResource};
    pub use super::initialization::{InitializationCoordinator, InitializationPhase};
    pub use super::required_settings::RequiredSettings;
    pub use super::scene::GodotScene;
    pub use super::scene::root_scripts::RootScripts;
    pub use super::scene::scene_root::SceneRoot;
    pub use super::scene_tree::SceneTreeSubsystem;
    pub use super::utils::{
        AsPhysicsSystem, AsVisualSystem, GodotPhysicsFrame, GodotVisualFrame,
        SystemDeltaTimerSubsystem,
    };
    pub use crate::action_framework::{
        ActionCheckStatus, ActionCheckStatusCodes, ActionInstanceId, ActionParams, ActionStatus,
        ActionStatusCodes, AllowanceSummary, Check, CheckAdapter, CheckReason, CheckReportLike,
        Criterion, ExecuteAction, ExecuteResult, ExecutionId,
    };
    pub use crate::app::{BevyApp, BevyAppSubsystem};
    pub use crate::dto::{BuildsDto, DataTransferConfig, WithGatherer, WithStateNode};
    pub use crate::export_queue::{ExportEventsPlugin, ExportMessagesPlugin, ExportQueue};
    pub use crate::import::{
        importers::EntityImporter, plugins::EntityInitializationPlugin,
        position::plugins::PositionInitializationPlugin, sets::PostEntityInitSet,
        subsystems::IdentitySubsystem,
    };
    pub use crate::performance::layer::{
        clear_benchmark_capture_gameplay_phase_for_current_scope,
        record_system_duration_for_current_scope,
        set_benchmark_capture_phase_for_current_scope, set_benchmark_capture_phase_for_scope,
        set_benchmark_capture_gameplay_phase_for_current_scope,
    };
    pub use crate::state::{BevyLifecyclePlugin, BevyLifecycleState};
    pub use crate::tools::collect_children;
    pub use bevy_godot4_proc_macros::{
        ExportComponent, ExportComponentNew, ExportEntity, action_pipeline, bevy_app,
        export_composed, export_queue, import_bundle, import_queue,
        settings_pipeline, with_state_node,
    };
}
pub use crate::app::APP_BUILDER_FN;
