//! Capture Bevy values for later delivery to Godot.

pub(crate) mod publication;
pub(crate) mod state;

#[doc(hidden)]
pub mod profiling;
/// Ordered outgoing event and message queues.
pub mod queues;
