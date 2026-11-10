//! Declare a reader's inputs once; generate its value fields and Godot callback arguments.
//! See README.md here for the build-time/runtime split and a complete update example.
//!
//! `prefix: Animation, inputs: { attack: Optional<AttackConfig> }` forwards
//! the input unchanged. Add `outputs: { current: f32 }` and
//! `value_mapping_fn: |health| Some(HealthValues { current: health.current })`
//! when preparing different values. Inputs and outputs keep declaration order.
//! Forwarded snapshots generate typed Godot arguments only for script consumers.
//! An optional `label` preserves a profiler name.
//!
//! `config: ExistingCombinedStateConfig, name: ExistingCombinedStateNode` still exposes a
//! handwritten config. Consumers connect to the standard subscribe method.

mod declaration;
mod generation;

use declaration::Spec;
use generation::generate;
use proc_macro::TokenStream;
use syn::parse_macro_input;

pub fn expand(input: TokenStream) -> TokenStream {
    // Validate the declaration before generating types and registration code.
    let spec = parse_macro_input!(input as Spec);
    generate(spec).into()
}
