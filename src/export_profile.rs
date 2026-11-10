//! Opt-in detail timings for the existing entity exporter.
//! Nested steps are charged once; only batch totals enter the shared profiler.

#[cfg(feature = "bridge-transport-profiling")]
use enabled::{Batch, begin, finish, measure as measure_enabled};

/// Meaningful steps of the entity-to-Godot flow.
#[derive(Clone, Copy)]
pub enum Stage {
    Query,
    Resolve,
    Deduplicate,
    Snapshot,
    StoreBind,
    FieldCopy,
    Equality,
    Bookkeeping,
    Signal,
    Removal,
    Other,
    GodotRoute,
}

/// Keeps one exporter's detail totals together until it returns.
pub struct ExportProfile {
    #[cfg(feature = "bridge-transport-profiling")]
    previous: Option<Batch>,
    #[cfg(feature = "bridge-transport-profiling")]
    owner: &'static str,
    #[cfg(feature = "bridge-transport-profiling")]
    active: bool,
}

impl ExportProfile {
    /// Enable only in a measured build with BEVY_GODOT_ENTITY_PROFILE=1.
    #[inline]
    pub fn begin(_owner: &'static str) -> Self {
        #[cfg(feature = "bridge-transport-profiling")]
        {
            return begin(_owner);
        }
        #[cfg(not(feature = "bridge-transport-profiling"))]
        Self {}
    }
}

impl Drop for ExportProfile {
    fn drop(&mut self) {
        #[cfg(feature = "bridge-transport-profiling")]
        finish(self);
    }
}

/// Runs the same operation exactly once, with no timer in ordinary builds.
#[inline]
pub fn measure<T>(_stage: Stage, work: impl FnOnce() -> T) -> T {
    #[cfg(feature = "bridge-transport-profiling")]
    {
        return measure_enabled(_stage, work);
    }
    #[cfg(not(feature = "bridge-transport-profiling"))]
    work()
}

/// Times lazy query iteration without allocating a candidate buffer.
pub struct ProfiledIter<I> {
    inner: I,
}

#[inline]
pub fn query_iter<I: Iterator>(inner: I) -> ProfiledIter<I> {
    ProfiledIter { inner }
}

impl<I: Iterator> Iterator for ProfiledIter<I> {
    type Item = I::Item;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        measure(Stage::Query, || self.inner.next())
    }
}

#[cfg(feature = "bridge-transport-profiling")]
mod enabled {
    use crate::export_profile::{ExportProfile, Stage};
    use crate::performance::layer::{
        measurements_enabled, record_detail_duration_for_current_scope,
    };
    use std::cell::RefCell;
    use std::env::var;
    use std::sync::OnceLock;
    use std::time::{Duration, Instant};

    const LABEL_LIST: [&str; 12] = [
        "query scan",
        "service lookup",
        "duplicate filtering",
        "identity and snapshot",
        "store borrowing",
        "capture snapshot fields",
        "compare queued snapshots",
        "revision and state bookkeeping",
        "queue entity notices",
        "removal cleanup",
        "other and probe overhead",
        "find Godot unit and set revision",
    ];
    thread_local! {
        static CURRENT: RefCell<Option<Batch>> = const { RefCell::new(None) };
    }
    static REQUESTED: OnceLock<bool> = OnceLock::new();

    pub struct Batch {
        total_list: [Duration; 12],
        stage: Stage,
        last: Instant,
    }

    impl Batch {
        fn transition(&mut self, next: Stage, now: Instant) -> Stage {
            self.total_list[self.stage as usize] += now.duration_since(self.last);
            self.last = now;
            let previous = self.stage;
            self.stage = next;
            previous
        }
    }

    pub fn begin(owner: &'static str) -> ExportProfile {
        let active = measurements_enabled()
            && *REQUESTED.get_or_init(|| var("BEVY_GODOT_ENTITY_PROFILE").as_deref() == Ok("1"));
        let previous = if active {
            CURRENT.with(|current| {
                current.borrow_mut().replace(Batch {
                    total_list: [Duration::ZERO; 12],
                    stage: Stage::Other,
                    last: Instant::now(),
                })
            })
        } else {
            None
        };
        ExportProfile {
            previous,
            owner,
            active,
        }
    }

    struct ReturnToStage(Option<Stage>);
    impl Drop for ReturnToStage {
        fn drop(&mut self) {
            if let Some(previous) = self.0 {
                CURRENT.with(|current| {
                    if let Some(batch) = current.borrow_mut().as_mut() {
                        batch.transition(previous, Instant::now());
                    }
                });
            }
        }
    }

    #[inline]
    pub fn measure<T>(stage: Stage, work: impl FnOnce() -> T) -> T {
        let previous = CURRENT.with(|current| {
            current
                .borrow_mut()
                .as_mut()
                .map(|batch| batch.transition(stage, Instant::now()))
        });
        let _return_to_stage = ReturnToStage(previous);
        work()
    }

    pub fn finish(scope: &mut ExportProfile) {
        if !scope.active {
            return;
        }
        let batch = CURRENT.with(|current| {
            let mut current = current.borrow_mut();
            let mut batch = current.take().expect("export profile must own its batch");
            batch.transition(Stage::Other, Instant::now());
            *current = scope.previous.take();
            batch
        });
        // Flush only once per stage, outside the measured pipeline. The enclosing
        // system still includes this recording overhead for the on/off comparison.
        for (label, duration) in LABEL_LIST.iter().zip(batch.total_list) {
            if !duration.is_zero() {
                let name = format!("entity export {} :: {}", scope.owner, label);
                record_detail_duration_for_current_scope(&name, duration);
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use crate::export_profile::Stage;
        use crate::export_profile::enabled::Batch;
        use std::time::{Duration, Instant};

        #[test]
        fn nested_stages_partition_elapsed_time_without_double_counting() {
            let start = Instant::now();
            let mut batch = Batch {
                total_list: [Duration::ZERO; 12],
                stage: Stage::Other,
                last: start,
            };
            batch.transition(Stage::StoreBind, start + Duration::from_millis(2));
            let parent = batch.transition(Stage::FieldCopy, start + Duration::from_millis(3));
            batch.transition(parent, start + Duration::from_millis(7));
            batch.transition(Stage::Other, start + Duration::from_millis(10));
            assert_eq!(
                batch.total_list[Stage::Other as usize],
                Duration::from_millis(2)
            );
            assert_eq!(
                batch.total_list[Stage::StoreBind as usize],
                Duration::from_millis(4)
            );
            assert_eq!(
                batch.total_list[Stage::FieldCopy as usize],
                Duration::from_millis(4)
            );
            assert_eq!(
                batch.total_list.iter().sum::<Duration>(),
                Duration::from_millis(10)
            );
        }
    }
}
