//! Tools for using Bevy inside a Godot scene.
//!
//! The bridge helps Godot and Bevy share data without either side reaching into
//! the other. It can import data from Godot, show Bevy state in Godot, and run
//! actions started by Godot. See the README first when you are new to the
//! library.

// Erase the entire probe, including its arguments, from ordinary release builds.
#[cfg(any(
    all(debug_assertions, feature = "dev-observability"),
    feature = "profiled-release"
))]
macro_rules! observe { ($($body:tt)*) => { $($body)* }; }
#[cfg(not(any(
    all(debug_assertions, feature = "dev-observability"),
    feature = "profiled-release"
)))]
macro_rules! observe {
    ($($body:tt)*) => {};
}

pub extern crate self as bevy_godot4;
pub use godot;

/// Requests that Godot starts and Bevy checks and performs.
pub mod actions;
mod app;
mod data;
mod debug;
mod entities;
/// Capturing outgoing state, events and messages.
pub mod export;
mod import;
mod performance;
/// The supported imports for game code using the bridge.
pub mod prelude;
mod scene;
mod updates;
