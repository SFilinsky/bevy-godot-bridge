//! Tools for using Bevy inside a Godot scene.
//!
//! The bridge helps Godot and Bevy share data without either side reaching into
//! the other. It can import data from Godot, show Bevy state in Godot, and run
//! actions started by Godot. See the README first when you are new to the
//! library.

// Erase the entire probe, including its arguments, from ordinary release builds.
#[cfg(any(
    all(debug_assertions, feature = "dev-observability"),
    feature = "profiled-release"
))]
macro_rules! observe { ($($body:tt)*) => { $($body)* }; }
#[cfg(not(any(
    all(debug_assertions, feature = "dev-observability"),
    feature = "profiled-release"
)))]
macro_rules! observe {
    ($($body:tt)*) => {};
}

pub extern crate self as bevy_godot4;
pub use godot;

/// Types used by actions that Godot starts and Bevy performs.
pub mod action_framework;
mod app;
mod app_action_queue;
#[cfg(feature = "assets")]
mod assets;
mod bevy_app_subsystem;
mod data_transfer;
mod debug;
mod entity_meta;
mod entity_registry;
mod erased_gd;
#[doc(hidden)]
pub mod export_profile;
mod export_publication;
/// Queues Bevy values until Godot reads a batch.
pub mod export_queue;
mod export_snapshots;
mod export_values;
mod import;
mod initialization;
mod occurrence_stream;
mod performance;
mod queue_source;
mod required_settings;
mod scene;
mod scene_tree;
mod state;
mod state_publication;
mod tools;
mod transfer_types;
mod update_dispatcher;
mod update_sources;
mod utils;

/// The normal imports for game code using this bridge.
///
/// Start with this module instead of importing private bridge files. Its macros
/// and traits are the supported ways to extend the bridge.
pub mod prelude {
//! The supported imports for game code using the bridge.
pub use crate::action_framework::{
    ActionCheckStatus, ActionCheckStatusCodes, ActionInstanceId, ActionParams, ActionStatus,
    ActionStatusCodes, AllowanceSummary, Check, CheckAdapter, CheckReason, CheckReportLike,
    Criterion, ExecuteAction, ExecuteResult, ExecutionId,
};
pub use crate::initialization::{InitializationCoordinator, InitializationPhase};
pub use crate::app_action_queue::AppAction;
pub use crate::state::{BevyLifecyclePlugin, BevyLifecycleState};
pub use crate::utils::{
    AsPhysicsSystem, AsVisualSystem, GodotPhysicsFrame, GodotVisualFrame, SystemDeltaTimerSubsystem,
};
pub use crate::app::{BevyApp, BevyAppSubsystem};
pub use crate::data_transfer::{BuildsDto, DataTransferConfig, WithGatherer};
pub use crate::transfer_types::{DtoOf, TransferDto, TransferSnapshot};
pub use crate::debug::cuboids::subsystem::{CuboidConfig, DebugCuboidSubsystem};
pub use crate::debug::debug_manager::{DebugRenderGateSubsystem, EDebugState};
pub use crate::debug::heatmap::subsystem::{DebugHeatmapSubsystem, HeatmapConfig};
pub use crate::debug::paths::subsystem::{DebugPathSubsystem, PathConfig};
pub use crate::debug::plugins::DebugVisualizationPlugin;
pub use crate::entity_meta::EntityMeta;
pub use crate::entity_registry::EntityRegistry;
pub use crate::export_publication::ExportPublication;
pub use crate::queue_source::QueueSource;
pub use crate::export_queue::{ExportEventsPlugin, ExportMessagesPlugin, ExportQueue};
pub use crate::state_publication::StatePublication;
pub use crate::export_snapshots::SnapshotPair;
pub use crate::import::queue::{ImportQueue, ImportQueuePlugin};
pub use crate::required_settings::RequiredSettings;
pub use crate::import::{
    importers::EntityImporter, plugins::EntityInitializationPlugin,
    position::plugins::PositionInitializationPlugin, sets::PostEntityInitSet,
    subsystems::IdentitySubsystem,
};
pub use crate::performance::layer::{
    MeasurementTimer, clear_benchmark_capture_gameplay_phase_for_current_scope, measure_work,
    measurements_enabled, record_system_duration_for_current_scope,
    set_benchmark_capture_gameplay_phase_for_current_scope,
    set_benchmark_capture_phase_for_current_scope, set_benchmark_capture_phase_for_scope,
};
pub use crate::scene::GodotScene;
pub use crate::tools::collect_children;
pub use crate::erased_gd::{ErasedGd, ErasedGdResource};
pub use crate::scene::node_registry::{NodeRegistry, RegistryError};
pub use crate::scene::registration::{child_services, register_store_on_entry};
pub use crate::scene::root_scripts::RootScripts;
pub use crate::scene::scene_root::SceneRoot;
pub use crate::scene_tree::SceneTreeSubsystem;
pub use crate::update_dispatcher::{ExportChangeSource, UpdateDispatcher};
pub use crate::occurrence_stream::OccurrenceStream;
pub use crate::update_sources::{
    CombinedStateConfig, ExportConfig, ExportState, Optional, Required, RequiredPair, StateInput,
    StateInputs, StateMappingFn, StateReadError, read_combined_state, report_state_error,
    subscribe_source, subscribe_source_id,
};
pub use crate::export_values::{ExportValue, ExportValueBatch, UpdateSubscription};
pub use bevy_godot4_proc_macros::bridge_dto;
pub use bevy_godot4_proc_macros::{
    action_pipeline, bevy_app_profiles, combined_state, export_composed, export_queue,
    import_bundle, import_queue, settings_pipeline, with_state_node,
};
pub use godot_apis::updates::{UpdatePayload, UpdateReceiver};

}
