//! Raw completed-update samples; Godot owns window calculations and wording.
use godot::builtin::{PackedFloat64Array, PackedInt64Array};
use godot::classes::RefCounted;
use godot::obj::{Base, Gd, NewGd};
use godot::prelude::GodotClass;
observe! { use crate::performance::update_history::UpdateHistory; }

/// Parallel arrays share an index. Sequence gaps mean unavailable observations.
#[derive(GodotClass)]
#[class(init, base = RefCounted)]
pub struct BevyUpdateSnapshot {
    base: Base<RefCounted>,
    #[var]
    available: bool,
    #[var]
    now_seconds: f64,
    #[var]
    retained_since_seconds: f64,
    #[var]
    sequence_list: PackedInt64Array,
    #[var]
    end_seconds_list: PackedFloat64Array,
    #[var]
    duration_ms_list: PackedFloat64Array,
}

impl BevyUpdateSnapshot {
    pub(crate) fn unavailable() -> Gd<Self> {
        Self::new_gd()
    }
}

observe! {
    impl BevyUpdateSnapshot {
        pub(crate) fn read(history: &UpdateHistory, after_sequence: i64) -> Gd<Self> {
            let mut result = Self::new_gd();
            {
                let mut target = result.bind_mut();
                target.available = history.enabled();
                target.now_seconds = history.now_seconds();
                target.retained_since_seconds = history.retained_since_seconds();
                for sample in history.samples_after(after_sequence.max(0) as u64) {
                    target.sequence_list.push(sample.sequence as i64);
                    target.end_seconds_list.push(sample.end_seconds);
                    target.duration_ms_list.push(sample.duration_ms);
                }
            }
            result
        }
    }
}
