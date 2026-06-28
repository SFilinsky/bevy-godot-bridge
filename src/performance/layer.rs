use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Instant;

use dashmap::DashMap;
use once_cell::sync::Lazy;
use tracing::field::{Field, Visit};
use tracing::{span, Id};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::{
    layer::{Context, Layer},
    registry::LookupSpan,
};

#[derive(Clone, Debug, Default)]
struct SpanInfo {
    /// The `name` field recorded on the span (what we use for display / key).
    name: Option<String>,
    /// True if this span represents an individual system execution.
    is_system: bool,
    /// True if this span represents a schedule execution.
    is_schedule: bool,
}

#[derive(Default)]
struct KindVisitor {
    name: Option<String>,
}

#[derive(Clone, Copy, Debug)]
struct SpanScopeId(AppScopeId);

impl Visit for KindVisitor {
    fn record_str(&mut self, field: &Field, value: &str) {
        if field.name() == "name" {
            self.name = Some(value.to_string());
        }
    }

    fn record_debug(&mut self, field: &Field, value: &dyn std::fmt::Debug) {
        if field.name() == "name" {
            self.name = Some(format!("{value:?}"));
        }
    }
}

/// Runtime statistics for a single item (schedule or system).
#[derive(Debug, Clone)]
pub struct SystemMetrics {
    /// Duration of the most recent invocation (seconds).
    pub last: f64,
    /// Total accumulated time (seconds).
    pub total: f64,
    /// Maximum observed execution time (seconds).
    pub max: f64,
    /// Number of times the item has run.
    pub calls: u64,

    /// EWMA estimates of "recent average" (seconds).
    pub ewma_1s: f64,
    pub ewma_5s: f64,
    pub ewma_30s: f64,

    /// Timestamp of the last sample update.
    pub last_seen: Option<Instant>,
}

impl Default for SystemMetrics {
    fn default() -> Self {
        Self {
            last: 0.0,
            total: 0.0,
            max: 0.0,
            calls: 0,
            ewma_1s: 0.0,
            ewma_5s: 0.0,
            ewma_30s: 0.0,
            last_seen: None,
        }
    }
}

/// Snapshot entry returned to consumers.
#[derive(Debug, Clone)]
pub struct SystemMetricsEntry {
    pub name: String,
    pub last: f64,
    pub avg: f64,
    pub max: f64,
    pub calls: u64,

    /// Recent averages (seconds)
    pub avg_1s: f64,
    pub avg_5s: f64,
    pub avg_30s: f64,

    /// True if this entry represents a system.
    pub is_system: bool,
    /// True if this entry represents a schedule.
    pub is_schedule: bool,
}

/// Global storage for all metrics, keyed by name.
pub type AppScopeId = u64;
pub const GLOBAL_APP_SCOPE_ID: AppScopeId = 0;

static NEXT_APP_SCOPE_ID: AtomicU64 = AtomicU64::new(1);
static CURRENT_APP_SCOPE_ID: AtomicU64 = AtomicU64::new(GLOBAL_APP_SCOPE_ID);

/// Metrics keyed by `(app_scope_id, span_name)`.
static METRICS: Lazy<DashMap<(AppScopeId, String), (SystemMetrics, SpanInfo)>> =
    Lazy::new(DashMap::new);
static BENCHMARK_CAPTURE_CLOCKS: Lazy<DashMap<AppScopeId, BenchmarkCaptureClock>> =
    Lazy::new(DashMap::new);
static BENCHMARK_CAPTURE_PHASE_BY_SCOPE: Lazy<DashMap<AppScopeId, BenchmarkPhaseKey>> =
    Lazy::new(DashMap::new);
static BENCHMARK_CAPTURED_SAMPLES: Lazy<
    DashMap<(AppScopeId, BenchmarkPhaseKey, String), (SpanInfo, Vec<BenchmarkInvocationSample>)>,
> = Lazy::new(DashMap::new);

const UNASSIGNED_BENCHMARK_PHASE_NAME: &str = "Unassigned";

#[derive(Debug, Clone)]
pub struct BenchmarkInvocationSample {
    pub at_seconds: f64,
    pub duration_seconds: f64,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BenchmarkPhaseKey {
    pub lifecycle_phase_name: String,
    pub gameplay_phase_name: Option<String>,
}

impl BenchmarkPhaseKey {
    pub fn from_lifecycle_phase_name(phase_name: impl Into<String>) -> Self {
        Self {
            lifecycle_phase_name: phase_name.into(),
            gameplay_phase_name: None,
        }
    }

    fn unassigned() -> Self {
        Self::from_lifecycle_phase_name(UNASSIGNED_BENCHMARK_PHASE_NAME)
    }
}

#[derive(Debug, Clone)]
struct BenchmarkCaptureClock {
    last_at: Instant,
    elapsed_seconds: f64,
    time_scale: f64,
}

#[derive(Debug, Clone)]
pub struct BenchmarkSystemSamples {
    pub phase_key: BenchmarkPhaseKey,
    pub name: String,
    pub is_system: bool,
    pub sample_list: Vec<BenchmarkInvocationSample>,
}

pub fn allocate_app_scope_id() -> AppScopeId {
    NEXT_APP_SCOPE_ID.fetch_add(1, Ordering::Relaxed)
}

pub struct AppScopeGuard {
    prev_scope_id: AppScopeId,
}

impl Drop for AppScopeGuard {
    fn drop(&mut self) {
        CURRENT_APP_SCOPE_ID.store(self.prev_scope_id, Ordering::Relaxed);
    }
}

pub fn enter_app_scope(scope_id: AppScopeId) -> AppScopeGuard {
    let prev_scope_id = CURRENT_APP_SCOPE_ID.swap(scope_id, Ordering::Relaxed);
    AppScopeGuard { prev_scope_id }
}

pub fn begin_benchmark_capture_for_scope(scope_id: AppScopeId, time_scale: f64) {
    clear_benchmark_samples_for_scope(scope_id);
    BENCHMARK_CAPTURE_CLOCKS.insert(
        scope_id,
        BenchmarkCaptureClock {
            last_at: Instant::now(),
            elapsed_seconds: 0.0,
            time_scale: sanitized_benchmark_time_scale(time_scale),
        },
    );
}

pub fn set_benchmark_capture_phase_for_current_scope(phase_name: impl Into<String>) {
    let scope_id = CURRENT_APP_SCOPE_ID.load(Ordering::Relaxed);
    set_benchmark_capture_phase_for_scope(scope_id, phase_name);
}

pub fn set_benchmark_capture_phase_for_scope(
    scope_id: AppScopeId,
    phase_name: impl Into<String>,
) {
    BENCHMARK_CAPTURE_PHASE_BY_SCOPE.insert(
        scope_id,
        BenchmarkPhaseKey::from_lifecycle_phase_name(phase_name),
    );
}

pub fn set_benchmark_capture_gameplay_phase_for_current_scope(phase_name: impl Into<String>) {
    let scope_id = CURRENT_APP_SCOPE_ID.load(Ordering::Relaxed);
    set_benchmark_capture_gameplay_phase_for_scope(scope_id, phase_name);
}

pub fn set_benchmark_capture_gameplay_phase_for_scope(
    scope_id: AppScopeId,
    phase_name: impl Into<String>,
) {
    let phase_name = phase_name.into();
    BENCHMARK_CAPTURE_PHASE_BY_SCOPE
        .entry(scope_id)
        .and_modify(|phase_key| {
            phase_key.gameplay_phase_name = Some(phase_name.clone());
        })
        .or_insert_with(|| BenchmarkPhaseKey {
            lifecycle_phase_name: UNASSIGNED_BENCHMARK_PHASE_NAME.to_string(),
            gameplay_phase_name: Some(phase_name),
        });
}

pub fn clear_benchmark_capture_gameplay_phase_for_current_scope() {
    let scope_id = CURRENT_APP_SCOPE_ID.load(Ordering::Relaxed);
    clear_benchmark_capture_gameplay_phase_for_scope(scope_id);
}

pub fn clear_benchmark_capture_gameplay_phase_for_scope(scope_id: AppScopeId) {
    let Some(mut phase_key) = BENCHMARK_CAPTURE_PHASE_BY_SCOPE.get_mut(&scope_id) else {
        return;
    };

    phase_key.gameplay_phase_name = None;
}

pub fn set_benchmark_capture_time_scale_for_scope(scope_id: AppScopeId, time_scale: f64) {
    let Some(mut clock) = BENCHMARK_CAPTURE_CLOCKS.get_mut(&scope_id) else {
        return;
    };

    let now = Instant::now();
    advance_benchmark_capture_clock(&mut clock, now);
    clock.time_scale = sanitized_benchmark_time_scale(time_scale);
}

pub fn benchmark_capture_phase_key_for_scope(scope_id: AppScopeId) -> Option<BenchmarkPhaseKey> {
    BENCHMARK_CAPTURE_PHASE_BY_SCOPE
        .get(&scope_id)
        .map(|phase| phase.value().clone())
}

pub fn drain_benchmark_capture_for_scope(scope_id: AppScopeId) -> Vec<BenchmarkSystemSamples> {
    BENCHMARK_CAPTURE_CLOCKS.remove(&scope_id);
    BENCHMARK_CAPTURE_PHASE_BY_SCOPE.remove(&scope_id);

    let key_list: Vec<(AppScopeId, BenchmarkPhaseKey, String)> = BENCHMARK_CAPTURED_SAMPLES
        .iter()
        .filter_map(|entry| {
            if entry.key().0 == scope_id {
                Some(entry.key().clone())
            } else {
                None
            }
        })
        .collect();

    key_list
        .into_iter()
        .filter_map(|key| {
            BENCHMARK_CAPTURED_SAMPLES
                .remove(&key)
                .map(|((_, phase_key, name), (info, sample_list))| BenchmarkSystemSamples {
                    phase_key,
                    name,
                    is_system: info.is_system,
                    sample_list,
                })
        })
        .collect()
}

fn clear_benchmark_samples_for_scope(scope_id: AppScopeId) {
    let key_list: Vec<(AppScopeId, BenchmarkPhaseKey, String)> = BENCHMARK_CAPTURED_SAMPLES
        .iter()
        .filter_map(|entry| {
            if entry.key().0 == scope_id {
                Some(entry.key().clone())
            } else {
                None
            }
        })
        .collect();

    for key in key_list {
        BENCHMARK_CAPTURED_SAMPLES.remove(&key);
    }
    BENCHMARK_CAPTURE_PHASE_BY_SCOPE.remove(&scope_id);
}

fn advance_benchmark_capture_clock(clock: &mut BenchmarkCaptureClock, now: Instant) -> f64 {
    let delta_seconds = now
        .checked_duration_since(clock.last_at)
        .map(|duration| duration.as_secs_f64())
        .unwrap_or_default();
    clock.elapsed_seconds += delta_seconds * clock.time_scale;
    clock.last_at = now;
    clock.elapsed_seconds
}

fn sanitized_benchmark_time_scale(time_scale: f64) -> f64 {
    if time_scale.is_finite() && time_scale > 0.0 {
        time_scale
    } else {
        1.0
    }
}

/// Install the performance tracing layer.
///
/// Call this **once**, before constructing or running the Bevy `App`.
/// Safe to call multiple times (subsequent calls are ignored).
pub fn init_performance_tracing() {
    static INSTALLED: Lazy<()> = Lazy::new(|| {
        let subscriber = tracing_subscriber::registry().with(SystemPerformanceLayer);
        if let Err(err) = tracing::subscriber::set_global_default(subscriber) {
            eprintln!(
                "Failed to install performance tracing layer: {err}. \
                 This usually means a global tracing subscriber was already set \
                 (for example, by Bevy's `bevy_log` feature via `LogPlugin`). \
                 To use this layer, disable `bevy_log` and configure your own \
                 tracing subscriber that includes `SystemPerformanceLayer`."
            );
        }
    });

    Lazy::force(&INSTALLED);
}

/// Returns a snapshot of schedule entries (in execution order) and system entries (sorted by last time desc).
///
/// - Schedules are returned in a pre-defined order.
/// - Systems are returned sorted slowest -> fastest by last execution time.
pub fn get_grouped_metrics_for_scope(
    scope_id: AppScopeId,
) -> (Vec<SystemMetricsEntry>, Vec<SystemMetricsEntry>) {
    let mut schedules: Vec<SystemMetricsEntry> = Vec::new();
    let mut systems: Vec<SystemMetricsEntry> = Vec::new();

    for kv in METRICS.iter() {
        if kv.key().0 != scope_id {
            continue;
        }

        let (m, info) = kv.value();

        let e = SystemMetricsEntry {
            name: kv.key().1.clone(),
            last: m.last,
            avg: if m.calls > 0 {
                m.total / m.calls as f64
            } else {
                0.0
            },
            max: m.max,
            calls: m.calls,
            avg_1s: m.ewma_1s,
            avg_5s: m.ewma_5s,
            avg_30s: m.ewma_30s,
            is_system: info.is_system,
            is_schedule: info.is_schedule,
        };

        if e.is_schedule {
            schedules.push(e);
        } else if e.is_system {
            systems.push(e);
        }
    }

    // Keep schedule order stable-ish here; UI does the final fixed ordering.
    schedules.sort_by(|a, b| a.name.cmp(&b.name));
    systems.sort_by(|a, b| b.last.partial_cmp(&a.last).unwrap());

    (schedules, systems)
}

/// Backwards compatible: schedules first, then systems.
pub fn get_sorted_metrics() -> Vec<SystemMetricsEntry> {
    get_sorted_metrics_for_scope(GLOBAL_APP_SCOPE_ID)
}

pub fn get_sorted_metrics_for_scope(scope_id: AppScopeId) -> Vec<SystemMetricsEntry> {
    let (mut schedules, mut systems) = get_grouped_metrics_for_scope(scope_id);
    schedules.append(&mut systems);
    schedules
}

/// Tracing layer that records execution time of Bevy spans.
///
/// We only keep spans that are either:
/// - "system" spans (and key them by `name` field)
/// - "schedule" spans (and key them by `name` field)
///
/// Everything else is ignored.
///
/// Additionally:
/// - we filter out "schedule facilitator" systems (Main::run_main, FixedMain::run_fixed_main, etc.)
///   because they are not actionable "real systems" to optimize.
pub struct SystemPerformanceLayer;

impl SystemPerformanceLayer {
    #[inline]
    fn ewma_update(prev: f64, sample: f64, dt: f64, tau: f64) -> f64 {
        let alpha = 1.0 - (-dt / tau).exp();
        prev + alpha * (sample - prev)
    }

    #[inline]
    fn update_ewmas(metrics: &mut SystemMetrics, sample: f64, now: Instant) {
        let Some(last_seen) = metrics.last_seen else {
            metrics.ewma_1s = sample;
            metrics.ewma_5s = sample;
            metrics.ewma_30s = sample;
            metrics.last_seen = Some(now);
            return;
        };

        let dt = (now - last_seen).as_secs_f64().max(0.0);
        metrics.ewma_1s = Self::ewma_update(metrics.ewma_1s, sample, dt, 1.0);
        metrics.ewma_5s = Self::ewma_update(metrics.ewma_5s, sample, dt, 5.0);
        metrics.ewma_30s = Self::ewma_update(metrics.ewma_30s, sample, dt, 30.0);
        metrics.last_seen = Some(now);
    }

    /// Returns true if this "system name" is actually an internal scheduler / facilitator
    /// and should not be shown as an actionable system.
    #[inline]
    fn is_internal_schedule_facilitator_system(name: &str) -> bool {
        // Known schedule labels (if a "system" is named exactly like a schedule, ignore it).
        const SCHEDULE_LABELS: &[&str] = &[
            "Main",
            "PreStartup",
            "Startup",
            "PostStartup",
            "First",
            "PreUpdate",
            "RunFixedMainLoop",
            "FixedMain",
            "FixedFirst",
            "FixedPreUpdate",
            "FixedUpdate",
            "FixedPostUpdate",
            "FixedLast",
            "Update",
            "SpawnScene",
            "PostUpdate",
            "Last",
            // Optional / feature-dependent, but harmless:
            "StateTransition",
        ];
        if SCHEDULE_LABELS.iter().any(|s| *s == name) {
            return true;
        }

        // Exact matches or obvious suffixes. These are Bevy’s schedule runner systems.
        // (They often appear in traces as “systems” but aren’t where you optimize.)
        const EXACT: &[&str] = &["Main::run_main", "FixedMain::run_fixed_main"];
        if EXACT.iter().any(|s| *s == name) {
            return true;
        }

        // Substring matches for common patterns
        // (names may be fully qualified paths depending on Bevy version/features)
        if name.contains("::run_main") {
            return true;
        }
        if name.contains("::run_fixed_main") {
            return true;
        }
        if name.contains("RunFixedMainLoop") && name.contains("run") {
            return true;
        }

        false
    }
}

impl<S> Layer<S> for SystemPerformanceLayer
where
    S: tracing::Subscriber + for<'a> LookupSpan<'a>,
{
    fn on_new_span(&self, attrs: &span::Attributes<'_>, id: &Id, ctx: Context<'_, S>) {
        let Some(span) = ctx.span(id) else {
            return;
        };

        let meta_name = span.metadata().name();
        let is_system = meta_name == "system";
        let is_schedule = meta_name == "schedule";

        // Ignore everything except system/schedule spans.
        if !is_system && !is_schedule {
            return;
        }

        // Capture the `name` field for actual system/schedule label.
        let mut visitor = KindVisitor::default();
        attrs.record(&mut visitor);

        let Some(name) = visitor.name else {
            return;
        };

        // Filter out internal “scheduler systems” early.
        if is_system && Self::is_internal_schedule_facilitator_system(&name) {
            return;
        }

        span.extensions_mut().insert(SpanInfo {
            name: Some(name),
            is_system,
            is_schedule,
        });
    }

    fn on_enter(&self, id: &Id, ctx: Context<'_, S>) {
        if let Some(span) = ctx.span(id) {
            // Only time spans that we classified (system/schedule)
            if span.extensions().get::<SpanInfo>().is_none() {
                return;
            }

            let scope_id = CURRENT_APP_SCOPE_ID.load(Ordering::Relaxed);
            let mut ext = span.extensions_mut();
            ext.insert(Instant::now());
            ext.insert(SpanScopeId(scope_id));
        }
    }

    fn on_exit(&self, id: &Id, ctx: Context<'_, S>) {
        let Some(span) = ctx.span(id) else {
            return;
        };

        // Read SpanInfo first (avoid extensions lock issues).
        let Some(info) = span.extensions().get::<SpanInfo>().cloned() else {
            return;
        };
        let Some(key) = info.name.clone() else {
            return;
        };

        // Then compute elapsed.
        let (elapsed, now, scope_id) = {
            let mut ext = span.extensions_mut();
            let Some(start) = ext.remove::<Instant>() else {
                return;
            };
            let scope_id = ext
                .remove::<SpanScopeId>()
                .map(|scope| scope.0)
                .unwrap_or(GLOBAL_APP_SCOPE_ID);
            let now = Instant::now();
            (start.elapsed().as_secs_f64(), now, scope_id)
        };

        let mut entry = METRICS
            .entry((scope_id, key.clone()))
            .or_insert_with(|| (SystemMetrics::default(), info.clone()));

        entry.value_mut().1 = info.clone();
        let metrics = &mut entry.value_mut().0;

        metrics.last = elapsed;
        metrics.total += elapsed;
        metrics.calls += 1;
        metrics.max = metrics.max.max(elapsed);

        Self::update_ewmas(metrics, elapsed, now);

        if let Some(mut capture_clock) = BENCHMARK_CAPTURE_CLOCKS.get_mut(&scope_id) {
            let at_seconds = advance_benchmark_capture_clock(&mut capture_clock, now);
            let phase_key = BENCHMARK_CAPTURE_PHASE_BY_SCOPE
                .get(&scope_id)
                .map(|phase| phase.value().clone())
                .unwrap_or_else(BenchmarkPhaseKey::unassigned);
            let mut captured_entry = BENCHMARK_CAPTURED_SAMPLES
                .entry((scope_id, phase_key, key.clone()))
                .or_insert_with(|| (info.clone(), Vec::new()));
            captured_entry.value_mut().0 = info;
            captured_entry
                .value_mut()
                .1
                .push(BenchmarkInvocationSample {
                    at_seconds,
                    duration_seconds: elapsed,
                });
        }
    }
}
