//! Performance tracing support for Bevy systems.
//!
//! This module installs a custom `tracing_subscriber` layer that records
//! execution times of *all* Bevy ECS systems (engine, user, and plugin
//! systems) when Bevy is built with the `trace` feature enabled.
//!
//! The collected metrics are stored globally and can be queried at runtime,
//! making them suitable for tight integration with the Godot editor
//! (custom inspector, debug UI, overlays, etc).

pub mod dto;
pub mod layer;
mod ui;

pub use layer::{SystemMetricsEntry, get_sorted_metrics, init_performance_tracing};
