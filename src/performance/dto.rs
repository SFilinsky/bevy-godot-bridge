use godot::builtin::{Array, GString};
use godot::classes::{Node, RefCounted};
use godot::obj::{Base, Gd, NewGd};
use godot::prelude::*;

use crate::performance::layer::{
    get_sorted_metrics, get_sorted_metrics_for_scope, AppScopeId, SystemMetricsEntry,
    GLOBAL_APP_SCOPE_ID,
};

/// Godot-facing DTO for a single system's performance metrics.
#[derive(GodotClass)]
#[class(init, base = RefCounted)]
pub struct SystemPerformanceEntryDto {
    #[base]
    base: Base<RefCounted>,

    /// Name (Bevy span `name` field).
    #[var]
    pub name: GString,

    /// Last execution time (milliseconds).
    #[var]
    pub last_ms: f64,

    /// Overall average time (milliseconds) across entire runtime.
    #[var]
    pub avg_ms: f64,

    /// Recent averages (milliseconds, EWMA approximations).
    #[var]
    pub avg_1s_ms: f64,
    #[var]
    pub avg_5s_ms: f64,
    #[var]
    pub avg_30s_ms: f64,

    /// Maximum execution time observed (milliseconds).
    #[var]
    pub max_ms: f64,

    /// Number of times the span has run.
    #[var]
    pub calls: i64,

    /// True if this entry represents a system.
    #[var]
    pub is_system: bool,
}

impl From<SystemMetricsEntry> for Gd<SystemPerformanceEntryDto> {
    fn from(e: SystemMetricsEntry) -> Self {
        let mut dto = SystemPerformanceEntryDto::new_gd();
        {
            let mut b = dto.bind_mut();
            b.name = GString::from(&e.name);
            b.last_ms = e.last * 1_000.0;
            b.avg_ms = e.avg * 1_000.0;
            b.avg_1s_ms = e.avg_1s * 1_000.0;
            b.avg_5s_ms = e.avg_5s * 1_000.0;
            b.avg_30s_ms = e.avg_30s * 1_000.0;
            b.max_ms = e.max * 1_000.0;
            b.calls = e.calls.min(i64::MAX as u64) as i64;
            b.is_system = e.is_system;
        }
        dto
    }
}

/// Godot-accessible entry point for querying Bevy system performance.
#[derive(GodotClass)]
#[class(init, base = Node)]
pub struct PerformanceMetrics {
    #[base]
    base: Base<Node>,
}

impl PerformanceMetrics {
    pub fn resolve(host: &Gd<Node>) -> Option<Gd<PerformanceMetrics>> {
        let Ok(app) = crate::app::BevyApp::resolve(host) else {
            return None;
        };

        app.try_get_node_as::<PerformanceMetrics>("PerformanceMetrics")
    }
}

#[godot_api]
impl PerformanceMetrics {
    fn resolve_scope_id(&self) -> AppScopeId {
        let host = self.base().clone().upcast::<Node>();
        let Ok(app) = crate::app::BevyApp::resolve(&host) else {
            return GLOBAL_APP_SCOPE_ID;
        };

        app.bind().performance_scope_id()
    }

    /// Returns an Array[SystemPerformanceEntryDto], with schedules first then systems.
    #[func]
    pub fn get_metrics(&self) -> Array<Gd<SystemPerformanceEntryDto>> {
        let scope_id = self.resolve_scope_id();
        let mut arr: Array<Gd<SystemPerformanceEntryDto>> = Array::new();
        for entry in get_sorted_metrics_for_scope(scope_id) {
            arr.push(&Gd::<SystemPerformanceEntryDto>::from(entry));
        }
        arr
    }

    /// Returns metrics that were collected outside a bound app scope.
    #[func]
    pub fn get_global_metrics(&self) -> Array<Gd<SystemPerformanceEntryDto>> {
        let mut arr: Array<Gd<SystemPerformanceEntryDto>> = Array::new();
        for entry in get_sorted_metrics() {
            arr.push(&Gd::<SystemPerformanceEntryDto>::from(entry));
        }
        arr
    }

    #[func]
    pub fn resolve_or_null(host: Gd<Node>) -> Option<Gd<PerformanceMetrics>> {
        Self::resolve(&host)
    }
}
