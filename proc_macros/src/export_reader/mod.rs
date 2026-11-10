//! Declare a reader's inputs once; generate its value fields and Godot callback arguments.
//!
//! `prefix: Animation, inputs: { attack: Optional<AttackConfig> }` forwards
//! the input unchanged. Add `outputs: { current: f32 }` and
//! `value_mapping_fn: |health| Some(HealthValues { current: health.current })`
//! when preparing different values. Inputs and outputs keep declaration order.
//! Forwarded fields must support Godot conversion; map plain Rust snapshots to
//! suitable output fields instead. An optional `label` preserves a profiler name.
//!
//! `config: ExistingReaderConfig, name: ExistingValueUpdates` still exposes a
//! handwritten config. Both forms accept extra feature-owned `methods`.

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
