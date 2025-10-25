//! Minimal demo of how to export data from Bevy to Godot.

use bevy::prelude::*;
use bevy_godot4::prelude::{export_bundle, DTO, DtoFrom};
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

impl DTO for TransformDto {
    type Component = Transform;
}

impl PartialEq for TransformDto {
    fn eq(&self, other: &Self) -> bool {
        self.translation == other.translation
    }
}

impl DtoFrom<Transform> for TransformDto {
    fn dto_from(t: &Transform) -> Gd<Self> {
        let mut dto = TransformDto::new_gd();
        {
            let mut d = dto.bind_mut();
            // Bevy -> Godot conversion
            d.translation = Vector3::new(t.translation.x, t.translation.y, t.translation.z);
        }
        dto
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

// Export all Transform-only entities (no tag)
export_bundle! {
    name: "Transform",
    dtos: [ TransformDto ],
}

// Export only entities tagged with UnitTag (e.g., your units)
export_bundle! {
    name: "Unit",
    tag:  UnitTag,
    dtos: [ TransformDto ],
}

// -----------------------------------------------------------------------------
// 4) Plugin that enables both exporters
// -----------------------------------------------------------------------------
pub struct ExportBindingsPlugin;

impl Plugin for ExportBindingsPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins((
            TransformEntityExportPlugin,
            UnitEntityExportPlugin,
        ));
    }
}

// -----------------------------------------------------------------------------
// 4) Attack TransformEntityExporter and UnitEntityExporter Nodes
//    To BevyApp and use signals to read entity updates.
// -----------------------------------------------------------------------------
