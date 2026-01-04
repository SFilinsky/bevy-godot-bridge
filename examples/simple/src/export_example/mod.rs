//! Minimal demo of how to export data from Bevy to Godot.

use bevy::prelude::*;
use bevy_godot4::DataTransferConfig;
use bevy_godot4::prelude::export_bundle;
use godot::prelude::*;

// -----------------------------------------------------------------------------
// 1) Tag component to scope "Unit" entities
// -----------------------------------------------------------------------------
#[derive(Component, Debug)]
pub struct UnitTag;

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

    fn update_data(dto: &Gd<Self::DtoType>, data: &mut Self::DataType) {
        let d = dto.bind();
        data.translation.x = d.translation.x;
        data.translation.y = d.translation.y;
        data.translation.z = d.translation.z;
    }

    fn update_dto(dto: &mut Gd<Self::DtoType>, data: &Self::DataType) {
        let mut d = dto.bind_mut();
        d.translation = Vector3::new(data.translation.x, data.translation.y, data.translation.z);
    }
}

// -----------------------------------------------------------------------------
// 3) Bundles to export
//
// The macro generates (per bundle):
// - <Name>EntityDto: wrapper with fields + per-field update flags
// - <Name>EntityExporter: Godot node emitting created/updated/removed
// - <Name>EntityExportPlugin: Bevy plugin wiring the export system
// - <Name>Entity / <Name>EntitySpawnHandler: optional helpers for Godot scenes
//
// Equality-based behavior:
// - Creation: all flags true
// - Update: flags true ONLY if the DTO value actually changed
// -----------------------------------------------------------------------------

// Export only entities tagged with UnitTag (e.g., your units)
export_bundle! {
    name: "Unit",
    tag:  UnitTag,
    components: [ TransformExportConfig ],
}

// -----------------------------------------------------------------------------
// 4) Plugin that enables both exporters
// -----------------------------------------------------------------------------
pub struct ExportBindingsPlugin;

impl Plugin for ExportBindingsPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(UnitEntityExportPlugin);
    }
}

// -----------------------------------------------------------------------------
// 5) Attach UnitEntityExporter Nodes
//    To BevyApp and use signals to read entity updates.
// -----------------------------------------------------------------------------
