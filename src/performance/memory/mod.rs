//! Samples the current game process once per wall-clock second, even while Godot is loading.
mod summary;
pub(crate) use summary::{CounterStats, MemoryReport};

use memory_stats::memory_stats;
use std::collections::VecDeque;
use std::env;
use std::sync::{
    Arc, Mutex, Weak,
    mpsc::{self, RecvTimeoutError, Sender},
};
use std::thread::{Builder, JoinHandle};
use std::time::{Duration, Instant};
use summary::Rollup;

pub(crate) const INTERVAL: Duration = Duration::from_secs(1);
const RAW_LIMIT: usize = 4096;

#[derive(Clone, Copy)]
pub(crate) struct Memory {
    pub godot_static: u64,
    pub resident: Option<usize>,
    pub private_commit: Option<usize>,
}

impl Memory {
    pub fn sample(godot_static: u64) -> Self {
        let process = memory_stats();
        Self {
            godot_static,
            resident: process.map(|sample| sample.physical_mem),
            #[cfg(windows)]
            private_commit: process.map(|sample| sample.virtual_mem),
            #[cfg(not(windows))]
            private_commit: None,
        }
    }
}

#[derive(Clone, Copy)]
struct Sample {
    at: Instant,
    memory: Memory,
    ready: bool,
}

impl Sample {
    fn take(godot_static: u64, ready: bool) -> Self {
        let memory = Memory::sample(godot_static);
        Self {
            at: Instant::now(),
            memory,
            ready,
        }
    }
}

#[derive(Default)]
struct Timeline {
    recent: VecDeque<Sample>,
    older: Rollup,
}

impl Timeline {
    fn push(&mut self, sample: Sample) {
        // A boundary read on the main thread can finish before an older worker sample arrives.
        if self.recent.back().is_some_and(|last| last.at > sample.at) {
            return;
        }
        if self.recent.len() == RAW_LIMIT {
            self.older.add(self.recent.pop_front().unwrap());
        }
        self.recent.push_back(sample);
    }
}

type Subscriber = Weak<Mutex<Timeline>>;
static WORKER: Mutex<Weak<Worker>> = Mutex::new(Weak::new());

struct Worker {
    subscribers: Arc<Mutex<Vec<Subscriber>>>,
    stop: Sender<()>,
    thread: Option<JoinHandle<()>>,
}

impl Worker {
    fn subscribe(timeline: &Arc<Mutex<Timeline>>) -> Arc<Self> {
        let mut shared = WORKER.lock().unwrap();
        let worker = shared.upgrade().unwrap_or_else(|| {
            let subscribers = Arc::new(Mutex::new(Vec::<Subscriber>::new()));
            let targets = subscribers.clone();
            let (stop, wake) = mpsc::channel();
            let thread = Builder::new()
                .name("process memory".into())
                .spawn(move || {
                    while wake.recv_timeout(INTERVAL) == Err(RecvTimeoutError::Timeout) {
                        let sample = Sample::take(0, false);
                        targets.lock().unwrap().retain(|target| {
                            if let Some(timeline) = target.upgrade() {
                                timeline.lock().unwrap().push(sample);
                                true
                            } else {
                                false
                            }
                        });
                    }
                })
                .ok();
            let worker = Arc::new(Self {
                subscribers,
                stop,
                thread,
            });
            *shared = Arc::downgrade(&worker);
            worker
        });
        let mut subscribers = worker.subscribers.lock().unwrap();
        subscribers.retain(|target| target.strong_count() > 0);
        subscribers.push(Arc::downgrade(timeline));
        drop(subscribers);
        worker
    }
}

impl Drop for Worker {
    fn drop(&mut self) {
        let _ = self.stop.send(());
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
    }
}

/// One measurement window. Overlapping directors share the operating-system read.
pub(crate) struct Window {
    timeline: Arc<Mutex<Timeline>>,
    worker: Option<Arc<Worker>>,
    ready: bool,
}

impl Window {
    pub fn start(ready: bool) -> Self {
        let timeline = Arc::new(Mutex::new(Timeline::default()));
        timeline.lock().unwrap().push(Sample::take(0, ready));
        let periodic =
            env::var("EQUILIBRIA_BENCHMARK_MEMORY_BOUNDARIES_ONLY").as_deref() != Ok("1");
        let worker = periodic.then(|| Worker::subscribe(&timeline));
        Self {
            timeline,
            worker,
            ready,
        }
    }

    pub fn mark_ready(&mut self, godot_static: u64) -> Option<Memory> {
        if self.ready {
            return None;
        }
        self.ready = true;
        let mut timeline = self.timeline.lock().unwrap();
        let sample = Sample::take(godot_static, true);
        timeline.push(sample);
        Some(sample.memory)
    }

    pub fn finish(self) -> MemoryReport {
        let mut timeline = self.timeline.lock().unwrap();
        timeline.push(Sample::take(0, false));
        let Timeline { older, recent } = &mut *timeline;
        for sample in recent.drain(..) {
            older.add(sample);
        }
        older.finish(self.worker.as_ref().is_some_and(|w| w.thread.is_some()))
    }
}
