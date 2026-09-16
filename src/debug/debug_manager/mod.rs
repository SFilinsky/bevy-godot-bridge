//! Scene-local debug-mode selection shared by Godot UI and Bevy systems.

use std::cmp::PartialEq;
use std::fmt::Debug;

use crate::app_action_queue::AppAction;
use crate::prelude::BevyApp;
use bevy::app::{App, Plugin};
use bevy::ecs::system::SystemParam;
use bevy::prelude::{Local, Res, Resource, Time, Timer, TimerMode};
use godot::classes::Node;
use godot::obj::Base;
use godot::prelude::*;

/// Visibility and rerender decision for one debug visualization system.
#[derive(Default, Clone, Copy)]
pub struct DebugRenderGateStatus {
    pub is_visible: bool,
    pub should_rerender: bool,
}

/// Local state used to rate-limit a debug visualization's refresh work.
#[derive(Default)]
pub struct DebugRenderGateState {
    pub timer: Timer,
    pub last_on: bool,
    pub initialized: bool,
}

/// Lets a Bevy debug system query its visibility and rerender cadence.
#[derive(SystemParam)]
pub struct DebugRenderGateSubsystem<'w, 's> {
    debug: Res<'w, DebugMode>,
    time: Res<'w, Time>,
    state: Local<'s, DebugRenderGateState>,
}

impl<'w, 's> DebugRenderGateSubsystem<'w, 's> {
    /// Returns whether a specific debug mode is active.
    pub fn is_visible(&self, state: EDebugState) -> bool {
        self.debug.current_state == state
    }

    pub fn get_status(
        &mut self,
        state: EDebugState,
        interval_seconds: f32,
    ) -> DebugRenderGateStatus {
        self.get_status_for_states(&[state], interval_seconds)
    }

    /// Returns one shared render status for diagnostics visible in any listed mode.
    pub fn get_status_for_states(
        &mut self,
        state_list: &[EDebugState],
        interval_seconds: f32,
    ) -> DebugRenderGateStatus {
        let currently_on = state_list
            .iter()
            .any(|state| self.debug.current_state == *state);

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

/// Debug modes selectable from authored Godot debug controls.
#[derive(GodotConvert, Var, Export, Clone, Default, Copy, PartialEq)]
#[godot(via = GString)]
#[derive(Debug)]
pub enum EDebugState {
    #[default]
    Off,
    Colliders,
    Navmesh,
    CaptureFlow,
    EnemyBuildingFlow,
}

impl Into<i32> for EDebugState {
    fn into(self) -> i32 {
        match self {
            EDebugState::Off => 0,
            EDebugState::Colliders => 1,
            EDebugState::Navmesh => 2,
            EDebugState::CaptureFlow => 3,
            EDebugState::EnemyBuildingFlow => 4,
        }
    }
}

impl From<i32> for EDebugState {
    fn from(value: i32) -> Self {
        match value {
            0 => EDebugState::Off,
            1 => EDebugState::Colliders,
            2 => EDebugState::Navmesh,
            3 => EDebugState::CaptureFlow,
            4 => EDebugState::EnemyBuildingFlow,
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
            EDebugState::CaptureFlow => GString::from("CaptureFlow"),
            EDebugState::EnemyBuildingFlow => GString::from("EnemyBuildingFlow"),
        }
    }
}

/// Authored Godot node that selects the scene-local Bevy debug mode.
#[derive(GodotClass)]
#[class(init, base=Node)]
pub struct DebugManager {
    #[export]
    current_debug_state: EDebugState,

    bevy_app: Option<Gd<BevyApp>>,

    #[base]
    base: Base<Node>,
}

struct SetDebugModeAction {
    new_state: EDebugState,
}

impl AppAction for SetDebugModeAction {
    fn apply(self: Box<Self>, app: &mut App) {
        if let Some(mut mode) = app.world_mut().get_resource_mut::<DebugMode>() {
            mode.current_state = self.new_state;
        }
    }
}

#[godot_api]
impl DebugManager {
    /// Resolves the debug manager below the nearest scene-local `BevyApp`.
    pub fn resolve(host: &Gd<Node>) -> Option<Gd<DebugManager>> {
        let Ok(app) = BevyApp::resolve(host) else {
            return None;
        };

        app.try_get_node_as::<DebugManager>("DebugManager")
    }

    pub(crate) fn current_state(&self) -> EDebugState {
        self.current_debug_state
    }

    fn enqueue_state_to_bevy(&self, new_state: EDebugState) {
        let Some(mut bevy_app) = self.bevy_app.as_ref().cloned() else {
            godot_error!("DebugManager.enqueue_state_to_bevy() failed: BevyApp not bound");
            return;
        };

        bevy_app
            .bind_mut()
            .action_queue
            .add(SetDebugModeAction { new_state });
    }

    fn schedule_sync_state_to_bevy(&mut self) {
        let callable = Callable::from_object_method(&self.to_gd(), "_sync_state_to_bevy");
        let _ = callable.call_deferred(&[]);
    }

    #[func]
    fn _sync_state_to_bevy(&mut self) {
        self.enqueue_state_to_bevy(self.current_debug_state);
    }

    #[func]
    pub fn resolve_or_null(host: Gd<Node>) -> Option<Gd<DebugManager>> {
        Self::resolve(&host)
    }

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
    pub fn set_debug_capture_flow(&mut self) {
        self.set_debug_state(EDebugState::CaptureFlow);
    }

    #[func]
    pub fn set_debug_enemy_building_flow(&mut self) {
        self.set_debug_state(EDebugState::EnemyBuildingFlow);
    }

    #[func]
    pub fn next_debug_state(&mut self) {
        match self.current_debug_state {
            EDebugState::Off => self.set_debug_state(EDebugState::Colliders),
            EDebugState::Colliders => self.set_debug_state(EDebugState::Navmesh),
            EDebugState::Navmesh => self.set_debug_state(EDebugState::CaptureFlow),
            EDebugState::CaptureFlow => self.set_debug_state(EDebugState::EnemyBuildingFlow),
            EDebugState::EnemyBuildingFlow => self.set_debug_state(EDebugState::Off),
        }
    }

    #[func]
    pub fn set_debug_state(&mut self, new_state: EDebugState) {
        godot_print!(
            "DebugManager.set_debug_state({})",
            GString::from(new_state.clone())
        );

        self.current_debug_state = new_state.clone();
        self.enqueue_state_to_bevy(new_state);

        self.signals().on_debug_change().emit(new_state);
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

    #[func]
    pub fn is_debug_capture_flow(&self) -> bool {
        self.current_debug_state == EDebugState::CaptureFlow
    }

    #[func]
    pub fn is_debug_enemy_building_flow(&self) -> bool {
        self.current_debug_state == EDebugState::EnemyBuildingFlow
    }

    #[signal]
    fn on_debug_change(new_state: EDebugState);
}

#[godot_api]
impl INode for DebugManager {
    fn ready(&mut self) {
        let host = self.base().clone().upcast::<Node>();
        self.bevy_app = BevyApp::resolve(&host).ok();

        if self.bevy_app.is_some() {
            self.schedule_sync_state_to_bevy();
        }

        let current_state = self.current_debug_state;
        self.signals().on_debug_change().emit(current_state);
    }
}

#[derive(Resource, Debug, Clone, Copy, Default)]
/// Bevy resource holding the currently selected bridge debug mode.
pub struct DebugMode {
    pub current_state: EDebugState,
}

/// Installs the Bevy-side debug-mode resource.
pub struct DebugModeBridgePlugin;
impl Plugin for DebugModeBridgePlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<DebugMode>();
    }
}
