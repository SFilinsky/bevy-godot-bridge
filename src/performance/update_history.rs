//! Bounded raw observations of complete Bevy updates, kept independently per app.
use std::collections::VecDeque;
use std::time::Instant;

const CAPACITY: usize = 32_768;
const RETENTION_SECONDS: f64 = 30.0;

#[derive(Clone, Copy)]
pub(crate) struct Sample {
    pub sequence: u64,
    pub end_seconds: f64,
    pub duration_ms: f64,
}

pub(crate) struct UpdateHistory {
    origin: Instant,
    enabled: bool,
    sequence: u64,
    retained_since_seconds: f64,
    sample_list: VecDeque<Sample>,
}

impl UpdateHistory {
    pub fn new(enabled: bool) -> Self {
        Self {
            origin: Instant::now(),
            enabled,
            sequence: 0,
            retained_since_seconds: 0.0,
            sample_list: VecDeque::new(),
        }
    }

    pub fn begin(&self) -> Option<Instant> {
        self.enabled.then(Instant::now)
    }

    pub fn finish(&mut self, started: Option<Instant>) {
        if let Some(started) = started {
            let ended = Instant::now();
            self.record(
                ended.duration_since(self.origin).as_secs_f64(),
                ended.duration_since(started).as_secs_f64() * 1000.0,
            );
        }
    }

    pub fn enabled(&self) -> bool {
        self.enabled
    }

    pub fn now_seconds(&self) -> f64 {
        self.origin.elapsed().as_secs_f64()
    }
    pub fn retained_since_seconds(&self) -> f64 {
        self.retained_since_seconds
    }

    pub fn samples_after(&self, sequence: u64) -> impl Iterator<Item = Sample> + '_ {
        self.sample_list
            .iter()
            .filter(move |sample| sample.sequence > sequence)
            .copied()
    }

    fn record(&mut self, end_seconds: f64, duration_ms: f64) {
        // Drop expired entries before allocating, and cap pathological uncapped runs.
        while self
            .sample_list
            .front()
            .is_some_and(|s| s.end_seconds <= end_seconds - RETENTION_SECONDS)
            || self.sample_list.len() >= CAPACITY
        {
            if let Some(sample) = self.sample_list.pop_front() {
                self.retained_since_seconds = sample.end_seconds;
            }
        }
        self.sequence += 1;
        self.sample_list.push_back(Sample {
            sequence: self.sequence,
            end_seconds,
            duration_ms,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::{CAPACITY, UpdateHistory};

    #[test]
    fn retains_boundary_and_sequences_without_adding_nested_work() {
        let mut history = UpdateHistory::new(true);
        history.record(1.0, 5.0);
        history.record(30.0, 2.0);
        history.record(31.0, 3.0);
        let samples: Vec<_> = history.samples_after(1).collect();
        assert_eq!(samples.len(), 2);
        assert_eq!(samples[0].sequence, 2);
        assert_eq!(samples[1].duration_ms, 3.0);
    }

    #[test]
    fn bounds_memory_and_exposes_sequence_gap() {
        let mut history = UpdateHistory::new(true);
        for _ in 0..CAPACITY + 4 {
            history.record(1.0, 0.1);
        }
        assert_eq!(history.samples_after(0).count(), CAPACITY);
        assert_eq!(history.samples_after(0).next().unwrap().sequence, 5);
    }

    #[test]
    fn disabled_source_does_not_measure() {
        let mut history = UpdateHistory::new(false);
        history.finish(history.begin());
        assert_eq!(history.samples_after(0).count(), 0);
    }
}
