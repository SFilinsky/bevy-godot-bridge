use crate::performance::layer::set_benchmark_capture_phase_for_current_scope;
use bevy::app::{App, Plugin};
use bevy::prelude::*;

// Bevy lifecycle state shared by the bridge and gameplay crate.
#[derive(States, Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
pub enum BevyLifecycleState {
    #[default]
    Loading,
    Initialization,
    Playing,
}

// Benchmark scopes need readable phase names that match lifecycle state.
#[derive(Clone, Copy)]
enum BenchmarkLifecyclePhase {
    Loading,
    Initialization,
    Playing,
}

impl BenchmarkLifecyclePhase {
    fn from_lifecycle_state(state: BevyLifecycleState) -> Self {
        match state {
            BevyLifecycleState::Loading => Self::Loading,
            BevyLifecycleState::Initialization => Self::Initialization,
            BevyLifecycleState::Playing => Self::Playing,
        }
    }

    fn name(self) -> &'static str {
        match self {
            Self::Loading => "Loading",
            Self::Initialization => "Initialization",
            Self::Playing => "Playing",
        }
    }
}

fn update_benchmark_phase(state: Res<State<BevyLifecycleState>>) {
    let phase = BenchmarkLifecyclePhase::from_lifecycle_state(*state.get());
    set_benchmark_capture_phase_for_current_scope(phase.name());
}

pub struct BevyLifecyclePlugin;

impl Plugin for BevyLifecyclePlugin {
    fn build(&self, app: &mut App) {
        app.init_state::<BevyLifecycleState>()
            .add_systems(
                OnEnter(BevyLifecycleState::Loading),
                update_benchmark_phase,
            )
            .add_systems(
                OnEnter(BevyLifecycleState::Initialization),
                update_benchmark_phase,
            )
            .add_systems(
                OnEnter(BevyLifecycleState::Playing),
                update_benchmark_phase,
            );
    }
}
