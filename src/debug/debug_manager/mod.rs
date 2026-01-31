use once_cell::sync::Lazy;
use std::sync::atomic::{AtomicBool, Ordering};

use bevy::app::{App, FixedUpdate, Plugin};
use bevy::ecs::system::SystemParam;
use bevy::prelude::{Local, Res, ResMut, Resource, Time, Timer, TimerMode};
use godot::classes::Node;
use godot::obj::Base;
use godot::prelude::*;

#[derive(Default, Clone, Copy)]
pub struct DebugRenderGateStatus {
    pub is_visible: bool,
    pub should_rerender: bool,
}

#[derive(Default)]
pub struct DebugRenderGateState {
    pub timer: Timer,
    pub last_debug_on: bool,
    pub initialized: bool,
}

#[derive(SystemParam)]
pub struct DebugRenderGate<'w, 's> {
    debug: Res<'w, DebugMode>,
    time: Res<'w, Time>,
    state: Local<'s, DebugRenderGateState>,
}

impl<'w, 's> DebugRenderGate<'w, 's> {
    pub fn get_status(&mut self, interval_seconds: f32) -> DebugRenderGateStatus {
        if !self.state.initialized {
            self.state.timer = Timer::from_seconds(interval_seconds, TimerMode::Repeating);
            self.state.last_debug_on = self.debug.on;
            self.state.initialized = true;
            return DebugRenderGateStatus {
                is_visible: self.debug.on,
                should_rerender: true,
            };
        }

        let debug_changed = self.state.last_debug_on != self.debug.on;
        self.state.last_debug_on = self.debug.on;

        if debug_changed {
            self.state.timer = Timer::from_seconds(interval_seconds, TimerMode::Repeating);
            return DebugRenderGateStatus {
                is_visible: self.debug.on,
                should_rerender: true,
            };
        }

        self.state.timer.tick(self.time.delta());

        DebugRenderGateStatus {
            is_visible: self.debug.on,
            should_rerender: self.state.timer.just_finished(),
        }
    }
}

/// Process-wide flag readable from Bevy.
static DEBUG_FLAG: Lazy<AtomicBool> = Lazy::new(|| AtomicBool::new(false));

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

    #[func]
    pub fn is_on(&self) -> bool {
        self.is_debug_on
    }

    #[signal]
    fn on_debug_change(is_on: bool);
}

#[godot_api]
impl INode for DebugManager {
    fn ready(&mut self) {
        let is_debug_on = self.is_debug_on;

        set_debug_flag(is_debug_on);

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
    res.on = get_debug_flag();
}

pub struct DebugModeBridgePlugin;
impl Plugin for DebugModeBridgePlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<DebugMode>()
            .add_systems(FixedUpdate, sync_debug_mode_from_godot);
    }
}
