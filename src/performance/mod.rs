//! Tools for measuring how long Bevy systems take to run.
//!
//! Turn on Bevy's `trace` feature, then call [`init_performance_tracing`] before
//! creating the Bevy app. The bridge records timings for Bevy's own systems,
//! your systems, and systems added by plugins. Godot can read the results to
//! show them in a debug panel or overlay.
//!
//! These tools collect numbers only. Your game decides which timings are good
//! enough.

pub mod dto;
pub mod layer;
mod benchmark;
mod benchmark_output_handlers;
mod ui;

pub use layer::init_performance_tracing;
