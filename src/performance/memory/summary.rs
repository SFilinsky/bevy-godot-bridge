//! Summarizes how much real time the process spent at each observed memory level.
use super::Sample;
use std::collections::BTreeMap;
use std::mem::take;
use std::time::{Duration, Instant};

#[derive(Default)]
pub(crate) struct CounterStats {
    pub start: Option<usize>,
    pub end: Option<usize>,
    pub peak: Option<(usize, Duration)>,
    pub samples: usize,
    pub covered: Duration,
    area: f64,
    // Each bucket keeps the largest real reading and the duration spent in it.
    buckets: BTreeMap<usize, (usize, Duration)>,
}

impl CounterStats {
    fn add(&mut self, value: Option<usize>, dt: Duration, elapsed: Duration, first: bool) {
        if let Some(previous) = self.end.filter(|_| !dt.is_zero()) {
            self.area += previous as f64 * dt.as_secs_f64();
            self.covered += dt;
            // Keep 8 significant bits. Storage stays bounded even in a multi-day run.
            let shift = previous.checked_ilog2().unwrap_or(0).saturating_sub(7);
            let bucket = self
                .buckets
                .entry((previous >> shift) << shift)
                .or_default();
            bucket.0 = bucket.0.max(previous);
            bucket.1 += dt;
        }
        if first {
            self.start = value;
        }
        self.end = value;
        if let Some(bytes) = value {
            self.samples += 1;
            if self.peak.is_none_or(|(peak, _)| bytes > peak) {
                self.peak = Some((bytes, elapsed));
            }
        }
    }

    pub fn average(&self) -> Option<usize> {
        (!self.covered.is_zero()).then(|| (self.area / self.covered.as_secs_f64()).round() as usize)
    }

    pub fn percentile(&self, percent: u32) -> Option<usize> {
        let target = (self.covered.as_nanos() * u128::from(percent)).div_ceil(100);
        let mut total = 0;
        for &(bytes, duration) in self.buckets.values() {
            total += duration.as_nanos();
            if total >= target {
                return Some(bytes);
            }
        }
        None
    }
}

#[derive(Default)]
pub(crate) struct Period {
    pub resident: CounterStats,
    pub private_commit: CounterStats,
    pub count: usize,
    pub duration: Duration,
    last: Option<Instant>,
}

impl Period {
    fn add(&mut self, sample: Sample, elapsed: Duration) {
        let dt = self.last.map_or(Duration::ZERO, |at| sample.at - at);
        self.duration += dt;
        self.resident
            .add(sample.memory.resident, dt, elapsed, self.count == 0);
        self.private_commit
            .add(sample.memory.private_commit, dt, elapsed, self.count == 0);
        self.count += 1;
        self.last = Some(sample.at);
    }
}

#[derive(Default)]
pub(crate) struct MemoryReport {
    pub whole_run: Period,
    pub loading: Period,
    pub gameplay: Period,
    pub periodic: bool,
}

#[derive(Default)]
pub(super) struct Rollup {
    first: Option<Instant>,
    report: MemoryReport,
}

impl Rollup {
    pub(super) fn add(&mut self, sample: Sample) {
        let elapsed = sample.at - *self.first.get_or_insert(sample.at);
        self.report.whole_run.add(sample, elapsed);
        if self.report.gameplay.count == 0 {
            self.report.loading.add(sample, elapsed);
        }
        // The ready reading closes loading and establishes the gameplay baseline.
        if sample.ready || self.report.gameplay.count > 0 {
            self.report.gameplay.add(sample, elapsed);
        }
    }

    pub(super) fn finish(&mut self, periodic: bool) -> MemoryReport {
        self.report.periodic = periodic;
        take(&mut self.report)
    }
}
#[cfg(test)]
mod tests {
    use super::{Rollup, Sample};
    use crate::performance::memory::{Memory, RAW_LIMIT, Timeline};
    use std::time::{Duration, Instant};

    fn sample(start: Instant, seconds: usize, resident: Option<usize>, ready: bool) -> Sample {
        Sample {
            at: start + Duration::from_secs(seconds as u64),
            memory: Memory {
                resident,
                private_commit: None,
                godot_static: 0,
            },
            ready,
        }
    }

    #[test]
    fn split_at_ready_and_weight_readings_by_wall_time() {
        let start = Instant::now();
        let mut rollup = Rollup::default();
        for (seconds, bytes, ready) in [
            (0, 40, false),
            (2, 80, true),
            (12, 200, false),
            (13, 20, false),
        ] {
            rollup.add(sample(start, seconds, Some(bytes), ready));
        }
        let report = rollup.finish(true);
        assert_eq!(report.loading.resident.average(), Some(40));
        assert_eq!(report.gameplay.resident.average(), Some(91));
        assert_eq!(report.gameplay.resident.percentile(50), Some(80));
        assert_eq!(report.gameplay.resident.percentile(95), Some(200));
        assert_eq!(
            report.gameplay.resident.peak,
            Some((200, Duration::from_secs(12)))
        );
        assert_eq!(report.gameplay.resident.start, Some(80));
        assert_eq!(report.gameplay.resident.end, Some(20));
        assert_eq!(report.whole_run.count, 4);
    }

    #[test]
    fn missing_reads_exclude_unknown_intervals_and_preserve_missing_endpoints() {
        let start = Instant::now();
        let mut rollup = Rollup::default();
        for (seconds, bytes) in [
            (0, None),
            (3, Some(100)),
            (5, None),
            (9, Some(7)),
            (9, None),
        ] {
            rollup.add(sample(start, seconds, bytes, false));
        }
        let counter = rollup.finish(true).whole_run.resident;
        assert_eq!(counter.covered, Duration::from_secs(2));
        assert_eq!(counter.average(), Some(100));
        assert_eq!((counter.start, counter.end), (None, None));
    }

    #[test]
    fn long_runs_preserve_an_early_peak_and_ready_boundary_after_raw_samples_roll_over() {
        let start = Instant::now();
        let mut trace = Timeline::default();
        for n in 0..=RAW_LIMIT + 2 {
            trace.push(sample(
                start,
                n,
                Some(if n == 0 { 900 } else { 100 }),
                n == 1,
            ));
        }
        assert_eq!(trace.recent.len(), RAW_LIMIT);
        for reading in trace.recent {
            trace.older.add(reading);
        }
        let report = trace.older.finish(true);
        assert_eq!(report.whole_run.count, RAW_LIMIT + 3);
        assert_eq!(report.whole_run.resident.peak, Some((900, Duration::ZERO)));
        assert_eq!(report.gameplay.resident.percentile(95), Some(100));
    }
}
