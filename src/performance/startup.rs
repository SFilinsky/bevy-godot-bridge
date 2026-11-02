//! Keeps a bounded timeline for each measured level until its director builds a report.
use super::startup_memory::Memory;
use godot::classes::Time;
use std::env;
use std::time::{SystemTime, UNIX_EPOCH};
use godot::obj::Singleton;
use std::collections::HashMap;
use std::sync::{Arc, LazyLock, Mutex};
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread::{self, ThreadId};
use std::time::{Duration, Instant};

const LIMIT: usize = 16_384;
pub(crate) type LevelId = i64;
type Shared = Arc<Mutex<Recording>>;

#[derive(Clone, Copy, Eq, PartialEq)]
pub(crate) enum Kind { Stage, Detail, System, Schedule, Marker }

pub(crate) struct Sample {
    pub name: String,
    pub at: Duration,
    pub duration: Duration,
    pub kind: Kind,
    pub thread: ThreadId,
}

pub(crate) struct Recording {
    origin: Instant,
    tree_start: Option<Instant>,
    pub origin_name: &'static str,
    offset: Duration,
    pub samples: Vec<Sample>,
    pub dropped: usize,
    pub endpoint: Option<&'static str>,
    pub memory: Option<Memory>,
    pub first_action: Option<Duration>,
}

impl Recording {
    fn new(origin_name: &'static str, offset: Duration) -> Self {
        Self { origin: Instant::now(), tree_start: None, origin_name, offset, samples: Vec::new(), dropped: 0, endpoint: None, memory: None, first_action: None }
    }

    fn record(&mut self, name: &str, kind: Kind, start: Instant, duration: Duration) {
        if self.endpoint.is_some() { return; }
        // Reserve space for boundaries even when thousands of systems or nodes ran.
        let limit = if matches!(kind, Kind::Detail | Kind::System | Kind::Schedule) { LIMIT - 128 } else { LIMIT };
        if self.samples.len() >= limit { self.dropped += 1; return; }
        self.samples.push(Sample {
            name: name.to_owned(), at: start.saturating_duration_since(self.origin) + self.offset,
            duration, kind, thread: thread::current().id(),
        });
    }
}

#[derive(Default)]
struct Sessions {
    pending: Option<Shared>,
    levels: HashMap<LevelId, Shared>,
    apps: HashMap<u64, LevelId>,
    seen_level: bool,
}

static SESSIONS: LazyLock<Mutex<Sessions>> = LazyLock::new(Mutex::default);
static OPEN: AtomicBool = AtomicBool::new(false);

fn session(sessions: &Sessions) -> Shared {
    if sessions.seen_level {
        Arc::new(Mutex::new(Recording::new("level load", Duration::ZERO)))
    } else {
        // Align the launcher once. All subsequent intervals use the same monotonic clock.
        let launch_offset = env::var("EQUILIBRIA_LAUNCH_UNIX_US").ok()
            .and_then(|s| s.parse::<u64>().ok())
            .and_then(|us| SystemTime::now().duration_since(UNIX_EPOCH).ok()?.checked_sub(Duration::from_micros(us)))
            .filter(|elapsed| *elapsed < Duration::from_secs(600));
        if let Some(offset) = launch_offset {
            return Arc::new(Mutex::new(Recording::new("launcher process start", offset)));
        }
        let offset = Duration::from_micros(Time::singleton().get_ticks_usec());
        Arc::new(Mutex::new(Recording::new("Godot engine startup", offset)))
    }
}

pub(crate) fn prepare_load() {
    let mut sessions = SESSIONS.lock().unwrap();
    sessions.pending = Some(session(&sessions));
}

pub(crate) fn begin_level(id: LevelId) {
    let mut sessions = SESSIONS.lock().unwrap();
    let capture = sessions.pending.take().unwrap_or_else(|| session(&sessions));
    capture.lock().unwrap().tree_start = Some(Instant::now());
    sessions.levels.insert(id, capture);
    sessions.seen_level = true;
    OPEN.store(true, Ordering::Relaxed);
}

pub(crate) fn tree_ready(id: LevelId) {
    let sessions = SESSIONS.lock().unwrap();
    if let Some(capture) = sessions.levels.get(&id) {
        let mut recording = capture.lock().unwrap();
        if let Some(start) = recording.tree_start.take() {
            recording.record("scene attachment", Kind::Stage, start, start.elapsed());
        }
    }
}

pub(crate) fn attach_app(id: u64, level: LevelId) {
    let mut sessions = SESSIONS.lock().unwrap();
    if sessions.levels.contains_key(&level) { sessions.apps.insert(id, level); }
}

fn for_app(app: u64) -> Option<Shared> {
    if !OPEN.load(Ordering::Relaxed) { return None; }
    let sessions = SESSIONS.lock().unwrap();
    sessions.levels.get(sessions.apps.get(&app)?).cloned()
}

pub(crate) struct Span {
    capture: Shared,
    name: String,
    kind: Kind,
    start: Instant,
}

impl Drop for Span {
    fn drop(&mut self) {
        let duration = self.start.elapsed();
        self.capture.lock().unwrap().record(&self.name, self.kind, self.start, duration);
    }
}

fn span(capture: Option<Shared>, kind: Kind, name: impl FnOnce() -> String) -> Option<Span> {
    let capture = capture?;
    if capture.lock().unwrap().endpoint.is_some() { return None; }
    Some(Span { capture, kind, name: name(), start: Instant::now() })
}

pub(crate) fn level_span(id: LevelId, kind: Kind, name: impl FnOnce() -> String) -> Option<Span> {
    if !OPEN.load(Ordering::Relaxed) { return None; }
    let capture = SESSIONS.lock().unwrap().levels.get(&id).cloned();
    span(capture, kind, name)
}

pub(crate) fn load_span(name: impl FnOnce() -> String) -> Option<Span> {
    let capture = SESSIONS.lock().unwrap().pending.clone();
    span(capture, Kind::Stage, name)
}

pub(crate) fn detail(id: u64, name: &str, start: Instant, end: Instant) {
    if let Some(capture) = for_app(id) {
        capture.lock().unwrap().record(name, Kind::Detail, start, end - start);
    }
}

pub(crate) fn app_span(id: u64, name: &'static str) -> Option<Span> {
    span(for_app(id), Kind::Stage, || name.to_owned())
}

pub(crate) fn system(id: u64, name: &str, start: Instant, end: Instant, is_system: bool) {
    if let Some(capture) = for_app(id) {
        let kind = if is_system { Kind::System } else { Kind::Schedule };
        capture.lock().unwrap().record(name, kind, start, end - start);
    }
}

pub(crate) fn marker(id: u64, name: &str) {
    if let Some(capture) = for_app(id) {
        capture.lock().unwrap().record(name, Kind::Marker, Instant::now(), Duration::ZERO);
    }
}

fn update_open(sessions: &Sessions) {
    OPEN.store(sessions.levels.values().any(|capture| capture.lock().unwrap().endpoint.is_none()), Ordering::Relaxed);
}

pub(crate) fn action_accepted(id: u64) {
    let sessions = SESSIONS.lock().unwrap();
    if let Some(capture) = sessions.apps.get(&id).and_then(|level| sessions.levels.get(level)) {
        let mut recording = capture.lock().unwrap();
        if recording.first_action.is_none() {
            recording.first_action = Some(recording.origin.elapsed() + recording.offset);
        }
    }
}

pub(crate) fn complete(id: u64, endpoint: &'static str, memory_bytes: u64) {
    let sessions = SESSIONS.lock().unwrap();
    if let Some(capture) = sessions.apps.get(&id).and_then(|level| sessions.levels.get(level)) {
        let mut recording = capture.lock().unwrap();
        if recording.endpoint.is_some() { return; }
        recording.record(endpoint, Kind::Marker, Instant::now(), Duration::ZERO);
        recording.endpoint = Some(endpoint);
        recording.memory = Some(Memory::at_ready(memory_bytes));
    }
    update_open(&sessions);
}

pub(crate) fn read<T>(app: u64, reader: impl FnOnce(&Recording) -> T) -> Option<T> {
    let sessions = SESSIONS.lock().unwrap();
    let recording = sessions.levels.get(sessions.apps.get(&app)?)?.lock().unwrap();
    Some(reader(&recording))
}

pub(crate) fn end_level(id: LevelId) {
    let mut sessions = SESSIONS.lock().unwrap();
    sessions.levels.remove(&id);
    sessions.apps.retain(|_, level| *level != id);
    update_open(&sessions);
}

#[cfg(test)]
mod tests {
    use super::{Kind, LIMIT, Recording};
    use std::time::{Duration, Instant};

    #[test]
    fn summary_orders_nested_work_by_start_and_preserves_overlap() {
        use super::super::startup_summary::Summary;
        let mut capture = Recording::new("test", Duration::ZERO);
        let start = capture.origin;
        // Children finish first. Output order follows entry, even across overlapping work.
        capture.record("lookup", Kind::Detail, start + Duration::from_millis(2), Duration::from_millis(4));
        capture.record("construct app", Kind::Stage, start + Duration::from_millis(1), Duration::from_millis(7));
        capture.record("attach scene", Kind::Stage, start, Duration::from_millis(10));
        let result = Summary::from_recording(&capture);
        assert_eq!(result.stages[0].name, "attach scene");
        assert_eq!(result.contributors[0].within.as_deref(), Some("construct app"));
        assert_eq!(result.last_ms, 10.0);
    }

    #[test]
    fn full_detail_buffer_preserves_readiness_and_closed_capture() {
        let mut capture = Recording::new("test", Duration::ZERO);
        for _ in 0..LIMIT { capture.record("lookup", Kind::Detail, Instant::now(), Duration::ZERO); }
        assert_eq!(capture.dropped, 128);
        capture.record("ready", Kind::Marker, Instant::now(), Duration::ZERO);
        capture.endpoint = Some("ready");
        let count = capture.samples.len();
        capture.record("late", Kind::Stage, Instant::now(), Duration::ZERO);
        assert_eq!(capture.samples.len(), count);
        assert_eq!(capture.samples.last().unwrap().name, "ready");
    }
}
