use once_cell::sync::Lazy;
use std::cmp::PartialEq;
use std::fmt::Debug;
use std::sync::atomic::{AtomicI32, Ordering};

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
    pub last_on: bool,
    pub initialized: bool,
}

#[derive(SystemParam)]
pub struct DebugRenderGate<'w, 's> {
    debug: Res<'w, DebugMode>,
    time: Res<'w, Time>,
    state: Local<'s, DebugRenderGateState>,
}

impl<'w, 's> DebugRenderGate<'w, 's> {
    pub fn get_status(
        &mut self,
        state: EDebugState,
        interval_seconds: f32,
    ) -> DebugRenderGateStatus {
        let currently_on = self.debug.current_state == state;

        if !self.state.initialized {
            self.state.timer = Timer::from_seconds(interval_seconds, TimerMode::Repeating);
            self.state.last_on = currently_on;
            self.state.initialized = true;
            return DebugRenderGateStatus {
                is_visible: currently_on,
                should_rerender: true,
            };
        }

        let debug_changed = self.state.last_on != currently_on;
        self.state.last_on = currently_on;

        if debug_changed {
            self.state.timer = Timer::from_seconds(interval_seconds, TimerMode::Repeating);
            return DebugRenderGateStatus {
                is_visible: currently_on,
                should_rerender: true,
            };
        }

        self.state.timer.tick(self.time.delta());

        DebugRenderGateStatus {
            is_visible: currently_on,
            should_rerender: self.state.timer.just_finished(),
        }
    }
}

/// Process-wide flag readable from Bevy.
static DEBUG_FLAG: Lazy<AtomicI32> = Lazy::new(|| AtomicI32::new(0));

pub fn get_debug_flag() -> EDebugState {
    DEBUG_FLAG.load(Ordering::Relaxed).into()
}

fn set_debug_flag(new_state: EDebugState) {
    DEBUG_FLAG.store(new_state.into(), Ordering::Relaxed);
}

#[derive(GodotConvert, Var, Export, Clone, Default, Copy, PartialEq)]
#[godot(via = GString)]
#[derive(Debug)]
pub enum EDebugState {
    #[default]
    Off,
    Colliders,
    Navmesh,
}

impl Into<i32> for EDebugState {
    fn into(self) -> i32 {
        match self {
            EDebugState::Off => 0,
            EDebugState::Colliders => 1,
            EDebugState::Navmesh => 2,
        }
    }
}

impl From<i32> for EDebugState {
    fn from(value: i32) -> Self {
        match value {
            0 => EDebugState::Off,
            1 => EDebugState::Colliders,
            2 => EDebugState::Navmesh,
            _ => panic!("Invalid EDebugManagerState value: {}", value),
        }
    }
}

impl From<EDebugState> for GString {
    fn from(value: EDebugState) -> Self {
        match value {
            EDebugState::Off => GString::from("Off"),
            EDebugState::Colliders => GString::from("Colliders"),
            EDebugState::Navmesh => GString::from("Navmesh"),
        }
    }
}

#[derive(GodotClass)]
#[class(init, base=Node)]
pub struct DebugManager {
    #[export]
    current_debug_state: EDebugState,

    #[base]
    base: Base<Node>,
}

#[godot_api]
impl DebugManager {
    #[func]
    pub fn set_debug_off(&mut self) {
        self.set_debug_state(EDebugState::Off);
    }

    #[func]
    pub fn set_debug_colliders(&mut self) {
        self.set_debug_state(EDebugState::Colliders);
    }

    #[func]
    pub fn set_debug_navmesh(&mut self) {
        self.set_debug_state(EDebugState::Navmesh);
    }

    #[func]
    pub fn next_debug_state(&mut self) {
        match self.current_debug_state {
            EDebugState::Off => self.set_debug_state(EDebugState::Colliders),
            EDebugState::Colliders => self.set_debug_state(EDebugState::Navmesh),
            EDebugState::Navmesh => self.set_debug_state(EDebugState::Off),
        }
    }

    #[func]
    pub fn set_debug_state(&mut self, new_state: EDebugState) {
        godot_print!(
            "DebugManager.set_debug_state({})",
            GString::from(new_state.clone())
        );

        self.current_debug_state = new_state.clone();
        set_debug_flag(new_state);
        self.signals().on_debug_change().emit();
    }

    #[func]
    pub fn is_debug_off(&self) -> bool {
        self.current_debug_state == EDebugState::Off
    }

    #[func]
    pub fn is_debug_colliders(&self) -> bool {
        self.current_debug_state == EDebugState::Colliders
    }

    #[func]
    pub fn is_debug_navmesh(&self) -> bool {
        self.current_debug_state == EDebugState::Navmesh
    }

    #[signal]
    fn on_debug_change();
}

#[godot_api]
impl INode for DebugManager {
    fn ready(&mut self) {
        set_debug_flag(self.current_debug_state);

        self.signals().on_debug_change().emit();
    }
}

#[derive(Resource, Debug, Clone, Copy, Default)]
pub struct DebugMode {
    pub current_state: EDebugState,
}

fn sync_debug_mode_from_godot(mut res: ResMut<DebugMode>) {
    res.current_state = get_debug_flag();
}

pub struct DebugModeBridgePlugin;
impl Plugin for DebugModeBridgePlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<DebugMode>()
            .add_systems(FixedUpdate, sync_debug_mode_from_godot);
    }
}
