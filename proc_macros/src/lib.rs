//! The public macros for moving data between Bevy and Godot.
//!
//! Each macro solves one clear problem: starting Bevy, importing data, showing
//! Bevy state in Godot, or running an action. `DataTransferConfig` describes
//! how one Rust value becomes one Godot value; the macro chooses when that move
//! happens.

mod action_pipeline;
mod bevy_app_impl;
mod export_component_impl;
mod export_component_new_impl;
mod export_composed;
mod export_entity_impl;
mod import_bundle;
mod queue;
mod settings_pipeline;
mod with_state_node;

use proc_macro::TokenStream;

/// Marks the function that sets up Bevy for a Godot scene.
///
/// Put this on the function that adds Bevy plugins and systems. When the
/// `BevyApp` node enters the Godot scene, it calls this function to make its
/// own Bevy app. Do not use it to store data shared by every level.
#[proc_macro_attribute]
pub fn bevy_app(attr: TokenStream, item: TokenStream) -> TokenStream {
    bevy_app_impl::expand(attr, item)
}

/// Old way to export a component.
///
/// Do not use this in new code. Use `DataTransferConfig`, `with_state_node!`,
/// and `export_composed!` instead. It remains only so older code still builds.
#[deprecated(
    note = "legacy exporter; use DataTransferConfig, with_state_node!, and export_composed! instead"
)]
#[proc_macro_derive(ExportComponent, attributes(export_component, export))]
pub fn export_component(input: TokenStream) -> TokenStream {
    export_component_impl::expand(input)
}

/// Old way to export a component.
///
/// Do not use this in new code. Use `DataTransferConfig`, `with_state_node!`,
/// and `export_composed!` instead. It remains only so older code still builds.
#[deprecated(
    note = "legacy exporter; use DataTransferConfig, with_state_node!, and export_composed! instead"
)]
#[proc_macro_derive(ExportComponentNew)]
pub fn export_component_new(input: TokenStream) -> TokenStream {
    export_component_new_impl::expand(input)
}

/// Old way to export an entity.
///
/// Do not use this in new code. Use `export_composed!` instead. It remains only
/// so older code still builds.
#[deprecated(
    note = "legacy exporter; use export_composed! with DataTransferConfig instead"
)]
#[proc_macro_derive(ExportEntity, attributes(export_entity))]
pub fn export_entity(input: TokenStream) -> TokenStream {
    export_entity_impl::expand(input)
}

/// Sends selected parts of a Bevy entity to Godot.
///
/// Use this when Godot needs the latest value, such as health or faction. It
/// updates Godot when an entity is created, changed, or removed. It does not
/// keep every past change, so do not use it for things such as every gold gain.
#[proc_macro]
pub fn export_composed(input: TokenStream) -> TokenStream {
    export_composed::expand(input)
}

/// Sends data authored in a Godot scene to a Bevy `Message`.
///
/// Use this while a scene is starting. It reads the values set in Godot and
/// writes the `Message` named in the macro. Your game decides what reads that
/// message and whether it creates an entity. It is not for repeated input.
#[proc_macro]
pub fn import_bundle(input: TokenStream) -> TokenStream {
    import_bundle::expand(input)
}

/// Creates a queue that sends requests from Godot to Bevy.
///
/// Godot puts data into the queue. Bevy takes the data out later and turns it
/// into a Rust value with `DataTransferConfig`. Each queue belongs to one
/// `BevyApp`. Use it for separate requests whose order matters.
#[proc_macro]
pub fn import_queue(input: TokenStream) -> TokenStream {
    queue::import_queue::expand(input)
}

/// Sends one set of Godot settings to Bevy while the scene starts.
///
/// Use it for settings edited in Godot that Bevy needs before it can start
/// gameplay. Do not use it for changing entity state or repeated requests.
#[proc_macro]
pub fn settings_pipeline(input: TokenStream) -> TokenStream {
    settings_pipeline::expand(input)
}

/// Creates a Godot node that stores one kind of entity data.
///
/// Pair it with `DataTransferConfig` and `export_composed!`. The node stores
/// the current and previous values for each entity. It also gives Godot a typed
/// way to ask whether an entity has this kind of data.
#[proc_macro]
pub fn with_state_node(input: TokenStream) -> TokenStream {
    with_state_node::expand(input)
}

/// Creates the full flow for a Godot action handled by Bevy.
///
/// Use this when Godot starts an action that Bevy must check and then perform.
/// For example, building a structure can have options, rules that allow or
/// reject it, and a final result. Do not use it for a simple one-time notice.
#[proc_macro]
pub fn action_pipeline(input: TokenStream) -> TokenStream {
    action_pipeline::expand(input)
}
