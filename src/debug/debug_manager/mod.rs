use once_cell::sync::Lazy;
use std::sync::atomic::{AtomicBool, Ordering};
use bevy::app::{App, FixedUpdate, Plugin};
use bevy::prelude::{ResMut, Resource};
use godot::classes::Node;
use godot::obj::{Base};
use godot::prelude::*;

/// Process-wide flag readable from Bevy.
static DEBUG_FLAG: Lazy<AtomicBool> = Lazy::new(|| AtomicBool::new(false));

/// Read current debug flag (for Bevy bridge).
pub fn get_debug_flag() -> bool {
    DEBUG_FLAG.load(Ordering::Relaxed)
}

fn set_debug_flag(new_state: bool) {
    DEBUG_FLAG.store(new_state, Ordering::Relaxed);
}

#[derive(GodotClass)]
#[class(init, base=Node)]
pub struct DebugManager {
    #[export]
    is_debug_on: bool,

    #[base]
    base: Base<Node>,
}

#[godot_api]
impl DebugManager {
    /// Switch debug mode. Emits `on_debug_change(is_on: bool)` if it changes.
    #[func]
    pub fn set_debug(&mut self, on: bool) {
        godot_print!("DebugManager.set_debug({})", on);

        if self.is_debug_on == on {
            return;
        }
        self.is_debug_on = on;
        self.signals().on_debug_change().emit(on);
        set_debug_flag(on);
    }

    /// Query current state for GDScript convenience.
    #[func]
    pub fn is_on(&self) -> bool {
        self.is_debug_on
    }

    /// Godot-side signal so scenes can bind show/hide logic, etc.
    #[signal]
    fn on_debug_change(is_on: bool);
}

#[godot_api]
impl INode for DebugManager {
    fn ready(&mut self) {
        let is_debug_on = self.is_debug_on;

        set_debug_flag(is_debug_on);

        // Initialize the global flag and emit initial value.
        self.signals().on_debug_change().emit(is_debug_on);
    }
}

#[derive(Resource, Debug, Clone, Copy)]
pub struct DebugMode {
    pub on: bool,
}

impl Default for DebugMode {
    fn default() -> Self {
        Self { on: false }
    }
}

fn sync_debug_mode_from_godot(mut res: ResMut<DebugMode>) {
    // Read from the atomic flag set by the Godot DebugManager.
    res.on = get_debug_flag();
}

pub struct DebugModeBridgePlugin;
impl Plugin for DebugModeBridgePlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<DebugMode>()
            // Run in FixedUpdate so gameplay/physics see a stable value each tick.
            .add_systems(FixedUpdate, sync_debug_mode_from_godot);
    }
}
