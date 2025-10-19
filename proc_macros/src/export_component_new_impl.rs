// src/export_for_derive.rs

use proc_macro::TokenStream;
use heck::ToSnakeCase;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput};

/// Derive macro: `#[derive(ExportFor)]`
///
/// Generates:
/// - Godot exporter node: `<Base>Exporter`
/// - Bevy system: `export_<base>_changes`
/// - Bevy plugin: `<Base>ExportPlugin`
///
/// Requirements on your DTO (local type):
/// - `impl DTO for YourDto { type Component = ... }`
/// - `impl DtoFrom<<YourDto as DTO>::Component> for YourDto { fn dto_from(&C)->Gd<Self> }`
///
/// Naming:
/// - `FooDto` → `FooExporter`, `export_foo_changes`, `FooExportPlugin`
pub fn expand(input: TokenStream) -> TokenStream {
    // -------------------- PARSE --------------------
    let input: DeriveInput = parse_macro_input!(input as DeriveInput);
    let dto_ident = &input.ident;

    if !matches!(input.data, Data::Struct(_)) {
        return syn::Error::new_spanned(&input.ident, "`ExportFor` can only be derived on structs")
            .to_compile_error()
            .into();
    }

    // -------------------- TRANSFORM --------------------
    let dto_name = dto_ident.to_string();
    let base_name = dto_name.strip_suffix("Dto").unwrap_or(&dto_name).to_string();

    let exporter_ident = format_ident!("{base_name}Exporter");
    let plugin_ident = format_ident!("{base_name}ExportPlugin");
    let base_snake = base_name.to_snake_case();
    let system_ident = format_ident!("export_{}_changes", base_snake);

    // -------------------- EXPAND --------------------
    // NOTE: We do NOT re-emit the input struct (prevents duplicate type errors).
    let expanded = quote! {
        // Hoisted imports used by generated items
        use bevy::prelude::*;
        use bevy_godot4::prelude::*;
        use bevy_godot4::godot::prelude::*;
        use std::collections::HashSet;

        // --- Godot exporter node with signals (docs included) ---
        #[doc = "Godot exporter node for Bevy component changes."]
        #[doc = ""]
        #[doc = "Place this under `BevyAppSingleton`. It emits signals on Added/Changed/Removed"]
        #[doc = "for the Bevy component associated with the DTO that derived `ExportFor`."]
        #[doc = ""]
        #[doc = "Signals:"]
        #[doc = "- `created(entity_id: i64, dto: Gd<DTO>)` — component was added this frame."]
        #[doc = "- `updated(entity_id: i64, dto: Gd<DTO>)` — component changed this frame."]
        #[doc = "- `removed(entity_id: i64)` — component removed this frame."]
        #[derive(GodotClass)]
        #[class(init, base=Node)]
        pub struct #exporter_ident {
            #[base]
            base: Base<Node>,
        }

        #[godot_api]
        impl #exporter_ident {
            #[doc = "Emitted when the component is **created** on an entity."]
            #[signal] fn created(entity_id: i64, dto: Gd<#dto_ident>);

            #[doc = "Emitted when the component is **updated** on an entity (frame where it changed)."]
            #[signal] fn updated(entity_id: i64, dto: Gd<#dto_ident>);

            #[doc = "Emitted when the component is **removed** from an entity."]
            #[signal] fn removed(entity_id: i64);
        }

        // --- Bevy system & plugin (convert via local DtoFrom trait) ---
        #[allow(non_snake_case)]
        fn #system_ident(
            created: Query<
                (Entity, &<#dto_ident as DTO>::Component),
                Added<<#dto_ident as DTO>::Component>
            >,
            updated: Query<
                (Entity, &<#dto_ident as DTO>::Component),
                Changed<<#dto_ident as DTO>::Component>
            >,
            mut removed: RemovedComponents<<#dto_ident as DTO>::Component>,
            mut scene_tree: SceneTreeRef,
        )
        where
            #dto_ident: DTO + DtoFrom<<#dto_ident as DTO>::Component>,
        {
            let Some(mut host) = scene_tree
                .get()
                .get_root()
                .unwrap()
                .get_node_or_null("BevyAppSingleton")
            else { return; };

            let mut exporters = collect_children::<#exporter_ident>(host, true);
            if exporters.is_empty() { return; }

            let mut created_ids: HashSet<u64> = HashSet::new();

            // Created
            for (entity, comp) in created.iter() {
                created_ids.insert(entity.to_bits());
                let dto = <#dto_ident as DtoFrom<<#dto_ident as DTO>::Component>>::dto_from(comp);
                for exporter in exporters.iter_mut() {
                    exporter.signals().created().emit(entity.to_bits() as i64, &dto);
                }
            }

            // Updated (skip same-frame inserts)
            for (entity, comp) in updated.iter() {
                if created_ids.contains(&entity.to_bits()) { continue; }
                let dto = <#dto_ident as DtoFrom<<#dto_ident as DTO>::Component>>::dto_from(comp);
                for exporter in exporters.iter_mut() {
                    exporter.signals().updated().emit(entity.to_bits() as i64, &dto);
                }
            }

            // Removed
            for entity in removed.read() {
                for exporter in exporters.iter_mut() {
                    exporter.signals().removed().emit(entity.to_bits() as i64);
                }
            }
        }

        /// Bevy plugin that installs the export system for this DTO’s component.
        ///
        /// Adds `PostUpdate` system `export_<base>_changes`.
        pub struct #plugin_ident;

        impl Plugin for #plugin_ident
        where
            #dto_ident: DTO + DtoFrom<<#dto_ident as DTO>::Component>,
        {
            fn build(&self, app: &mut App) {
                app.add_systems(PostUpdate, #system_ident);
            }
        }
    };

    expanded.into()
}
