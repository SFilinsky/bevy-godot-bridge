use std::time::Instant;

use dashmap::DashMap;
use once_cell::sync::Lazy;
use tracing::field::{Field, Visit};
use tracing::{Id, span};
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
static METRICS: Lazy<DashMap<String, (SystemMetrics, SpanInfo)>> = Lazy::new(DashMap::new);

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
pub fn get_grouped_metrics() -> (Vec<SystemMetricsEntry>, Vec<SystemMetricsEntry>) {
    let mut schedules: Vec<SystemMetricsEntry> = Vec::new();
    let mut systems: Vec<SystemMetricsEntry> = Vec::new();

    for kv in METRICS.iter() {
        let (m, info) = kv.value();

        let e = SystemMetricsEntry {
            name: kv.key().clone(),
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
    let (mut schedules, mut systems) = get_grouped_metrics();
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
            span.extensions_mut().insert(Instant::now());
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
        let (elapsed, now) = {
            let mut ext = span.extensions_mut();
            let Some(start) = ext.remove::<Instant>() else {
                return;
            };
            let now = Instant::now();
            (start.elapsed().as_secs_f64(), now)
        };

        let mut entry = METRICS
            .entry(key)
            .or_insert_with(|| (SystemMetrics::default(), info.clone()));

        entry.value_mut().1 = info;
        let metrics = &mut entry.value_mut().0;

        metrics.last = elapsed;
        metrics.total += elapsed;
        metrics.calls += 1;
        metrics.max = metrics.max.max(elapsed);

        Self::update_ewmas(metrics, elapsed, now);
    }
}
