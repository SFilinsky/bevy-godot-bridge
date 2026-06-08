use super::benchmark_output_handlers::{BenchmarkOutputHandler, YamlBenchmarkOutputHandler};
use super::dto::{PerformanceMetrics, SystemPerformanceEntryDto};
use super::layer::{
    AppScopeId, BenchmarkInvocationSample, BenchmarkSystemSamples, GLOBAL_APP_SCOPE_ID,
    begin_benchmark_capture_for_scope, drain_benchmark_capture_for_scope,
};
use godot::builtin::{Array, GString};
use godot::classes::{Engine, INode, Node, ProjectSettings};
use godot::global::{Error, godot_error, godot_print};
use godot::obj::{Base, Gd};
use godot::prelude::*;
use std::env;
use std::path::PathBuf;

const DEFAULT_REPORT_PATH: &str = "res://logs/benchmarks/benchmark_report.yml";
const OUTPUT_FILE_ENV_VAR: &str = "EQUILIBRIA_BENCHMARK_OUTPUT_FILE";
const TIME_SCALE_ENV_VAR: &str = "EQUILIBRIA_BENCHMARK_TIME_SCALE";
const FRAME_BUDGET_60_FPS_MS: f64 = 16.67;
const FRAME_BUDGET_30_FPS_MS: f64 = 33.33;
const FRAME_BUDGET_SLOW_MS: f64 = 50.0;
const FRAME_BUDGET_VERY_SLOW_MS: f64 = 100.0;

#[derive(Default)]
pub(crate) struct MetricSummary {
    pub(crate) total_ms: f64,
    pub(crate) average_ms: f64,
    pub(crate) min_ms: f64,
    pub(crate) max_ms: f64,
    pub(crate) p50_ms: f64,
    pub(crate) p90_ms: f64,
    pub(crate) p95_ms: f64,
    pub(crate) p99_ms: f64,
}

impl MetricSummary {
    fn from_sample_list(sample_list: &[f64]) -> Self {
        if sample_list.is_empty() {
            return Self::default();
        }

        let mut sorted_sample_list = sample_list.to_vec();
        sorted_sample_list.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

        let total_ms = sample_list.iter().sum::<f64>();
        let calls = sample_list.len() as f64;

        Self {
            total_ms,
            average_ms: total_ms / calls,
            min_ms: sorted_sample_list[0],
            max_ms: sorted_sample_list[sorted_sample_list.len() - 1],
            p50_ms: percentile_from_sorted_sample_list(&sorted_sample_list, 0.50),
            p90_ms: percentile_from_sorted_sample_list(&sorted_sample_list, 0.90),
            p95_ms: percentile_from_sorted_sample_list(&sorted_sample_list, 0.95),
            p99_ms: percentile_from_sorted_sample_list(&sorted_sample_list, 0.99),
        }
    }
}

#[derive(Default)]
pub(crate) struct FramesOverBudget {
    pub(crate) over_16_67_ms: usize,
    pub(crate) over_33_33_ms: usize,
    pub(crate) over_50_ms: usize,
    pub(crate) over_100_ms: usize,
}

impl FramesOverBudget {
    fn from_frame_time_list(frame_time_ms_list: &[f64]) -> Self {
        Self {
            over_16_67_ms: count_over_budget(frame_time_ms_list, FRAME_BUDGET_60_FPS_MS),
            over_33_33_ms: count_over_budget(frame_time_ms_list, FRAME_BUDGET_30_FPS_MS),
            over_50_ms: count_over_budget(frame_time_ms_list, FRAME_BUDGET_SLOW_MS),
            over_100_ms: count_over_budget(frame_time_ms_list, FRAME_BUDGET_VERY_SLOW_MS),
        }
    }
}

pub(crate) struct BenchmarkSystemReportStats {
    pub(crate) name: String,
    pub(crate) calls: usize,
    pub(crate) summary: MetricSummary,
    pub(crate) max_1s_average_ms: f64,
    pub(crate) max_5s_average_ms: f64,
    pub(crate) max_10s_average_ms: f64,
    pub(crate) share_of_total_percent: f64,
}

impl BenchmarkSystemReportStats {
    fn from_samples(samples: BenchmarkSystemSamples, cpu_time_total_ms: f64) -> Self {
        let duration_ms_list: Vec<f64> = samples
            .sample_list
            .iter()
            .map(|sample| sample.duration_seconds * 1_000.0)
            .collect();
        let summary = MetricSummary::from_sample_list(&duration_ms_list);
        let share_of_total_percent = if cpu_time_total_ms > 0.0 && samples.is_system {
            summary.total_ms / cpu_time_total_ms * 100.0
        } else {
            0.0
        };

        Self {
            name: samples.name,
            calls: samples.sample_list.len(),
            summary,
            max_1s_average_ms: max_average_in_window_ms(&samples.sample_list, 1.0),
            max_5s_average_ms: max_average_in_window_ms(&samples.sample_list, 5.0),
            max_10s_average_ms: max_average_in_window_ms(&samples.sample_list, 10.0),
            share_of_total_percent,
        }
    }
}

pub(crate) struct BenchmarkReport {
    pub(crate) scene_id: String,
    pub(crate) duration_seconds: f64,
    pub(crate) configured_duration_seconds: f64,
    pub(crate) time_scale: f64,
    pub(crate) frames_total: usize,
    pub(crate) frame_summary: MetricSummary,
    pub(crate) frames_over_budget: FramesOverBudget,
    pub(crate) cpu_time_total_ms: f64,
    pub(crate) metrics_refresh_count: u64,
    pub(crate) refresh_interval_seconds: f64,
    pub(crate) system_stat_list: Vec<BenchmarkSystemReportStats>,
}

/// Godot node that records Bevy system metrics and writes a benchmark report.
///
/// The node is intended to live under `BevyApp` next to `PerformanceMetrics`.
/// Benchmark scenes configure it through exported properties and can listen for
/// `benchmark_finished` when they want to unmount the scene themselves.
#[derive(GodotClass)]
#[class(base = Node)]
pub struct BenchmarkSceneDirector {
    #[export]
    scene_id: GString,

    #[export]
    duration_seconds: f64,

    #[export]
    refresh_interval_sec: f64,

    #[export]
    output_file_path: GString,

    #[export]
    quit_on_finish: bool,

    #[export]
    time_scale: f64,

    #[export]
    start_on_ready: bool,

    metrics: Option<Gd<PerformanceMetrics>>,
    elapsed_seconds: f64,
    frame_time_ms_list: Vec<f64>,
    metrics_refresh_count: u64,
    is_running: bool,
    is_finished: bool,
    is_metrics_connected: bool,
    performance_scope_id: AppScopeId,
    previous_time_scale: Option<f64>,
    output_handler_list: Vec<Box<dyn BenchmarkOutputHandler>>,

    #[base]
    base: Base<Node>,
}

impl BenchmarkSceneDirector {
    pub fn resolve(host: &Gd<Node>) -> Option<Gd<BenchmarkSceneDirector>> {
        let Ok(app) = crate::app::BevyApp::resolve(host) else {
            return None;
        };

        app.try_get_node_as::<BenchmarkSceneDirector>("BenchmarkSceneDirector")
    }
}

#[godot_api]
impl INode for BenchmarkSceneDirector {
    fn init(base: Base<Node>) -> Self {
        Self {
            scene_id: "benchmark_scene".into(),
            duration_seconds: 5.0,
            refresh_interval_sec: 0.25,
            output_file_path: DEFAULT_REPORT_PATH.into(),
            quit_on_finish: false,
            time_scale: 1.0,
            start_on_ready: false,
            metrics: None,
            elapsed_seconds: 0.0,
            frame_time_ms_list: Vec::new(),
            metrics_refresh_count: 0,
            is_running: false,
            is_finished: false,
            is_metrics_connected: false,
            performance_scope_id: GLOBAL_APP_SCOPE_ID,
            previous_time_scale: None,
            output_handler_list: vec![Box::new(YamlBenchmarkOutputHandler)],
            base,
        }
    }

    fn ready(&mut self) {
        self.apply_environment_overrides();
        if self.start_on_ready {
            self.start();
        }
    }

    fn process(&mut self, delta_seconds: f64) {
        if !self.is_running || self.is_finished {
            return;
        }

        self.elapsed_seconds += delta_seconds;
        self.frame_time_ms_list
            .push(self.real_delta_seconds(delta_seconds) * 1_000.0);
        if self.elapsed_seconds >= self.duration_seconds {
            self.finish();
        }
    }
}

#[godot_api]
impl BenchmarkSceneDirector {
    #[func]
    pub fn start(&mut self) {
        self.apply_time_scale();
        self.metrics = self.resolve_metrics();
        self.connect_metrics_source();
        self.performance_scope_id = self.resolve_performance_scope_id();

        if let Some(mut metrics) = self.metrics.as_ref().cloned() {
            metrics
                .bind_mut()
                .configure_refresh_interval_sec(self.refresh_interval_sec);
        }

        self.elapsed_seconds = 0.0;
        self.frame_time_ms_list.clear();
        self.metrics_refresh_count = 0;
        self.is_running = true;
        self.is_finished = false;
        begin_benchmark_capture_for_scope(self.performance_scope_id);
    }

    #[func]
    pub fn finish_now(&mut self) {
        if self.is_finished {
            return;
        }

        self.finish();
    }

    #[func]
    pub fn output_path(&self) -> GString {
        self.output_file_path.clone()
    }

    #[func]
    pub fn resolve_or_null(host: Gd<Node>) -> Option<Gd<BenchmarkSceneDirector>> {
        Self::resolve(&host)
    }

    #[func]
    fn _on_metrics_updated(&mut self, _entries: Array<Gd<SystemPerformanceEntryDto>>) {
        if !self.is_running || self.is_finished {
            return;
        }

        self.metrics_refresh_count += 1;
    }

    fn resolve_metrics(&mut self) -> Option<Gd<PerformanceMetrics>> {
        let host = self.base().clone().upcast::<Node>();
        PerformanceMetrics::resolve(&host)
    }

    fn resolve_performance_scope_id(&self) -> AppScopeId {
        let host = self.base().clone().upcast::<Node>();
        let Ok(app) = crate::app::BevyApp::resolve(&host) else {
            return GLOBAL_APP_SCOPE_ID;
        };

        app.bind().performance_scope_id()
    }

    fn apply_environment_overrides(&mut self) {
        if let Ok(output_file_path) = env::var(OUTPUT_FILE_ENV_VAR) {
            if !output_file_path.is_empty() {
                self.output_file_path = output_file_path.as_str().into();
            }
        }

        if let Ok(time_scale) = env::var(TIME_SCALE_ENV_VAR) {
            if let Ok(parsed_time_scale) = time_scale.parse::<f64>() {
                self.time_scale = parsed_time_scale;
            }
        }
    }

    fn connect_metrics_source(&mut self) {
        if self.is_metrics_connected {
            return;
        }

        let Some(metrics) = self.metrics.as_ref().cloned() else {
            godot_error!(
                "BenchmarkSceneDirector: failed to resolve PerformanceMetrics; benchmark report will be empty"
            );
            return;
        };

        let callback = self.base().callable("_on_metrics_updated");
        let mut metrics_node = metrics.upcast::<Node>();
        let error = metrics_node.connect("metrics_updated", &callback);
        if error != Error::OK {
            godot_error!(
                "BenchmarkSceneDirector: failed to connect metrics_updated signal: {:?}",
                error
            );
            return;
        }

        self.is_metrics_connected = true;
    }

    fn finish(&mut self) {
        self.is_running = false;
        self.is_finished = true;
        self.restore_time_scale();

        let captured_system_sample_list =
            drain_benchmark_capture_for_scope(self.performance_scope_id);
        let report = self.build_report(captured_system_sample_list);
        let output_path = self.global_output_path();
        let success = match self.write_report(&output_path, &report) {
            Ok(()) => {
                godot_print!(
                    "BenchmarkSceneDirector: report written to {}",
                    output_path.display()
                );
                true
            }
            Err(error) => {
                godot_error!(
                    "BenchmarkSceneDirector: failed to write report to {}: {}",
                    output_path.display(),
                    error
                );
                false
            }
        };

        self.signals().benchmark_finished().emit(
            &GString::from(output_path.to_string_lossy().as_ref()),
            success,
        );

        if self.quit_on_finish {
            if let Some(mut tree) = self.base().get_tree() {
                tree.quit();
            }
        }
    }

    fn global_output_path(&self) -> PathBuf {
        let path = self.output_file_path.to_string();
        if !path.starts_with("res://") && !path.starts_with("user://") {
            return PathBuf::from(path);
        }

        let project_settings = ProjectSettings::singleton();
        PathBuf::from(
            project_settings
                .globalize_path(&self.output_file_path)
                .to_string(),
        )
    }

    fn write_report(&self, output_path: &PathBuf, report: &BenchmarkReport) -> std::io::Result<()> {
        for output_handler in &self.output_handler_list {
            output_handler.write_report(output_path, report)?;
        }

        Ok(())
    }

    fn build_report(
        &self,
        captured_system_sample_list: Vec<BenchmarkSystemSamples>,
    ) -> BenchmarkReport {
        let frame_summary = MetricSummary::from_sample_list(&self.frame_time_ms_list);
        let frames_over_budget = FramesOverBudget::from_frame_time_list(&self.frame_time_ms_list);
        let cpu_time_total_ms = captured_system_sample_list
            .iter()
            .filter(|samples| samples.is_system)
            .flat_map(|samples| samples.sample_list.iter())
            .map(|sample| sample.duration_seconds * 1_000.0)
            .sum::<f64>();
        let mut system_stats_list: Vec<BenchmarkSystemReportStats> = captured_system_sample_list
            .into_iter()
            .filter(|samples| samples.is_system)
            .map(|samples| BenchmarkSystemReportStats::from_samples(samples, cpu_time_total_ms))
            .collect();
        system_stats_list.sort_by(|a, b| {
            b.summary
                .p99_ms
                .partial_cmp(&a.summary.p99_ms)
                .unwrap_or(std::cmp::Ordering::Equal)
                .then_with(|| {
                    b.summary
                        .total_ms
                        .partial_cmp(&a.summary.total_ms)
                        .unwrap_or(std::cmp::Ordering::Equal)
                })
                .then_with(|| a.name.cmp(&b.name))
        });

        BenchmarkReport {
            scene_id: self.scene_id.to_string(),
            duration_seconds: self.elapsed_seconds,
            configured_duration_seconds: self.duration_seconds,
            time_scale: self.time_scale,
            frames_total: self.frame_time_ms_list.len(),
            frame_summary,
            frames_over_budget,
            cpu_time_total_ms,
            metrics_refresh_count: self.metrics_refresh_count,
            refresh_interval_seconds: self.refresh_interval_sec,
            system_stat_list: system_stats_list,
        }
    }

    #[signal]
    fn benchmark_finished(output_file_path: GString, success: bool);

    fn apply_time_scale(&mut self) {
        let mut engine = Engine::singleton();
        if self.previous_time_scale.is_none() {
            self.previous_time_scale = Some(engine.get_time_scale());
        }

        engine.set_time_scale(sanitized_time_scale(self.time_scale));
    }

    fn restore_time_scale(&mut self) {
        let Some(previous_time_scale) = self.previous_time_scale.take() else {
            return;
        };

        Engine::singleton().set_time_scale(previous_time_scale);
    }

    fn real_delta_seconds(&self, scaled_delta_seconds: f64) -> f64 {
        let time_scale = sanitized_time_scale(self.time_scale);
        if time_scale <= 0.0 {
            return 0.0;
        }

        scaled_delta_seconds / time_scale
    }
}

fn sanitized_time_scale(time_scale: f64) -> f64 {
    if time_scale.is_finite() && time_scale > 0.0 {
        time_scale
    } else {
        1.0
    }
}

fn percentile_from_sorted_sample_list(sorted_sample_list: &[f64], percentile: f64) -> f64 {
    if sorted_sample_list.is_empty() {
        return 0.0;
    }

    let last_index = sorted_sample_list.len() - 1;
    let clamped_percentile = percentile.clamp(0.0, 1.0);
    let index = (clamped_percentile * last_index as f64).round() as usize;
    sorted_sample_list[index]
}

fn count_over_budget(sample_list: &[f64], budget_ms: f64) -> usize {
    sample_list
        .iter()
        .filter(|sample| **sample > budget_ms)
        .count()
}

fn max_average_in_window_ms(sample_list: &[BenchmarkInvocationSample], window_seconds: f64) -> f64 {
    if sample_list.is_empty() {
        return 0.0;
    }

    let mut sorted_sample_list = sample_list.to_vec();
    sorted_sample_list.sort_by(|a, b| {
        a.at_seconds
            .partial_cmp(&b.at_seconds)
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    let mut left = 0usize;
    let mut duration_sum_ms = 0.0;
    let mut max_average_ms = 0.0;

    for right in 0..sorted_sample_list.len() {
        duration_sum_ms += sorted_sample_list[right].duration_seconds * 1_000.0;

        while sorted_sample_list[right].at_seconds - sorted_sample_list[left].at_seconds
            > window_seconds
        {
            duration_sum_ms -= sorted_sample_list[left].duration_seconds * 1_000.0;
            left += 1;
        }

        let sample_count = right - left + 1;
        max_average_ms = f64::max(max_average_ms, duration_sum_ms / sample_count as f64);
    }

    max_average_ms
}
