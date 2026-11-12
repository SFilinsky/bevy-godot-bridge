use bevy::{
    ecs::{
        schedule::ScheduleConfigs,
        system::{ScheduleSystem, SystemParam},
    },
    prelude::*,
};
use std::{
    marker::PhantomData,
    time::{Duration, Instant},
};

/// Present while Godot runs `BevyApp` from its normal `_process` callback.
#[derive(Resource)]
pub struct GodotVisualFrame;

/// Present while Godot runs `BevyApp` from its `_physics_process` callback.
#[derive(Resource)]
pub struct GodotPhysicsFrame;

/// Runs a Bevy system only during Godot's physics updates.
pub trait AsPhysicsSystem<Params> {
    #[allow(clippy::wrong_self_convention)]
    fn as_physics_system(self) -> ScheduleConfigs<ScheduleSystem>;
}

impl<Params, T: IntoSystem<(), (), Params>> AsPhysicsSystem<Params> for T {
    fn as_physics_system(self) -> ScheduleConfigs<ScheduleSystem> {
        self.run_if(resource_exists::<GodotPhysicsFrame>)
    }
}

/// Runs a Bevy system only during Godot's normal frame updates.
pub trait AsVisualSystem<Params> {
    #[allow(clippy::wrong_self_convention)]
    fn as_visual_system(self) -> ScheduleConfigs<ScheduleSystem>;
}

impl<Params, T: IntoSystem<(), (), Params>> AsVisualSystem<Params> for T {
    fn as_visual_system(self) -> ScheduleConfigs<ScheduleSystem> {
        self.run_if(resource_exists::<GodotVisualFrame>)
    }
}

/// Gives a system the time since it last ran.
///
/// Not every system runs on every Bevy update, and Bevy can update multiple
/// times during one rendered frame.
#[derive(SystemParam)]
pub struct SystemDeltaTimerSubsystem<'w, 's> {
    last_time: Local<'s, Option<Instant>>,
    marker: PhantomData<&'w ()>,
}

impl SystemDeltaTimerSubsystem<'_, '_> {
    /// Returns the duration since this system parameter was last used.
    pub fn delta(&mut self) -> Duration {
        let now = Instant::now();
        let last_time = self.last_time.unwrap_or(now);

        *self.last_time = Some(now);

        now - last_time
    }

    /// Returns [`Self::delta`] in seconds as `f32`.
    pub fn delta_seconds(&mut self) -> f32 {
        self.delta().as_secs_f32()
    }

    /// Returns [`Self::delta`] in seconds as `f64`.
    pub fn delta_seconds_f64(&mut self) -> f64 {
        self.delta().as_secs_f64()
    }
}
