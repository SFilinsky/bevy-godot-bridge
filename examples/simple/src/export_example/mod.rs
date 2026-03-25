//! Minimal demo of how to export data from Bevy to Godot.

use bevy::prelude::*;
use bevy_godot4::prelude::{
    export_composed, with_state_node, DataTransferConfig, IdentitySubsystem,
};
use godot::prelude::*;

// -----------------------------------------------------------------------------
// 1) Tag component to scope "Unit" entities
// -----------------------------------------------------------------------------
#[derive(Component, Debug)]
pub struct UnitComposedTag;

// -----------------------------------------------------------------------------
// 2) DTO for Transform (what Godot receives)
//    IMPORTANT: derive PartialEq so the macro can compare prev/curr DTOs
// -----------------------------------------------------------------------------
#[derive(GodotClass, Debug)]
#[class(init, base = RefCounted)]
pub struct TransformDto {
    /// World position (we only export translation in this demo)
    #[var]
    pub translation: Vector3,

    #[base]
    base: Base<RefCounted>,
}

impl PartialEq for TransformDto {
    fn eq(&self, other: &Self) -> bool {
        self.translation == other.translation
    }
}

struct TransformExportConfig;
impl DataTransferConfig for TransformExportConfig {
    type DataType = Transform;
    type DtoType = TransformDto;

    fn update_data(
        dto: &Gd<Self::DtoType>,
        data: &mut Self::DataType,
        _identity: &mut IdentitySubsystem,
    ) {
        let d = dto.bind();
        data.translation.x = d.translation.x;
        data.translation.y = d.translation.y;
        data.translation.z = d.translation.z;
    }

    fn update_dto(
        dto: &mut Gd<Self::DtoType>,
        data: &Self::DataType,
        _identity: &mut IdentitySubsystem,
    ) {
        let mut d = dto.bind_mut();
        d.translation = Vector3::new(data.translation.x, data.translation.y, data.translation.z);
    }
}

with_state_node! {
    dto: TransformDto,
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
// 4) Attach UnitComposedEntityExporter Nodes
//    To BevyApp and use signals to read entity updates.
// -----------------------------------------------------------------------------
