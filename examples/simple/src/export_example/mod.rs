//! Minimal demo of how to export data from Bevy to Godot.

use bevy::prelude::{App, Component, Plugin, Transform, Vec3};
use bevy_godot4::prelude::{
    DataTransferConfig, IdentitySubsystem, bridge_dto, export_composed, with_state_node,
};
use godot::builtin::Vector3;

// -----------------------------------------------------------------------------
// 1) Tag component to scope "Unit" entities
// -----------------------------------------------------------------------------
#[derive(Component, Debug)]
pub struct UnitComposedTag;

// -----------------------------------------------------------------------------
// 2) Capture a Rust value; generate its Godot DTO from the same fields.
//    PartialEq lets the store skip positions that have not changed.
// -----------------------------------------------------------------------------
#[bridge_dto(TransformDto)]
#[derive(Debug, PartialEq)]
pub struct TransformSnapshot {
    /// World position (we only export translation in this demo)
    #[var]
    pub translation: Vector3,
}

struct TransformExportConfig;
impl DataTransferConfig for TransformExportConfig {
    type DataType = Transform;

    type Snapshot = TransformSnapshot;

    fn update_data(
        snapshot: &Self::Snapshot,
        data: &mut Self::DataType,
        _identity: &mut IdentitySubsystem,
    ) {
        // Apply a captured position without reading a Godot object.
        let position = snapshot.translation;
        data.translation = Vec3::new(position.x, position.y, position.z);
    }

    fn update_snapshot(
        snapshot: &mut Self::Snapshot,
        data: &Self::DataType,
        _identity: &mut IdentitySubsystem,
    ) {
        // Capture the position now; create a DTO only when a script reads it.
        let position = data.translation;
        snapshot.translation = Vector3::new(position.x, position.y, position.z);
    }
}

with_state_node! {
    config: TransformExportConfig,
    state: TransformStateNode,
}

export_composed! {
    name: "UnitComposed",
    tag: UnitComposedTag,
    components: [ TransformExportConfig ],
}

// -----------------------------------------------------------------------------
// 3) Plugin that enables composed exporter
// -----------------------------------------------------------------------------
pub struct ExportBindingsPlugin;

impl Plugin for ExportBindingsPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins((UnitComposedEntityExportPlugin,));
    }
}

// -----------------------------------------------------------------------------
// 4) Author UnitComposedEntityExporter and UpdateDispatcher below BevyApp.
//    Creation/removal signals discover entities. Subscribe to TransformStateNode
//    for position values; retain and cancel the subscription with its consumer.
// -----------------------------------------------------------------------------
