use super::dto::{PerformanceMetrics, SystemPerformanceEntryDto};
use godot::builtin::{Array, GString};
use godot::classes::{INode, Node, ProjectSettings};
use godot::global::{godot_error, godot_print, Error};
use godot::obj::{Base, Gd};
use godot::prelude::*;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

const DEFAULT_REPORT_PATH: &str = "res://logs/benchmarks/benchmark_report.yml";

#[derive(Default)]
struct BenchmarkSystemStats {
    name: String,
    is_system: bool,
    samples: u64,
    calls: i64,
    last_ms: f64,
    avg_ms: f64,
    max_ms: f64,
    max_last_ms: f64,
    max_avg_ms: f64,
    max_avg_1s_ms: f64,
    max_avg_5s_ms: f64,
    max_avg_30s_ms: f64,
}

impl BenchmarkSystemStats {
    fn record(&mut self, entry: &SystemPerformanceEntryDto) {
        self.name = entry.name.to_string();
        self.is_system = entry.is_system;
        self.samples += 1;
        self.calls = entry.calls;
        self.last_ms = entry.last_ms;
        self.avg_ms = entry.avg_ms;
        self.max_ms = entry.max_ms;
        self.max_last_ms = self.max_last_ms.max(entry.last_ms);
        self.max_avg_ms = self.max_avg_ms.max(entry.avg_ms);
        self.max_avg_1s_ms = self.max_avg_1s_ms.max(entry.avg_1s_ms);
        self.max_avg_5s_ms = self.max_avg_5s_ms.max(entry.avg_5s_ms);
        self.max_avg_30s_ms = self.max_avg_30s_ms.max(entry.avg_30s_ms);
    }
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

    metrics: Option<Gd<PerformanceMetrics>>,
    elapsed_seconds: f64,
    sample_count: u64,
    is_running: bool,
    is_finished: bool,
    is_metrics_connected: bool,
    stats_by_name: HashMap<String, BenchmarkSystemStats>,

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
            metrics: None,
            elapsed_seconds: 0.0,
            sample_count: 0,
            is_running: false,
            is_finished: false,
            is_metrics_connected: false,
            stats_by_name: HashMap::new(),
            base,
        }
    }

    fn ready(&mut self) {
        self.start();
    }

    fn process(&mut self, delta_seconds: f64) {
        if !self.is_running || self.is_finished {
            return;
        }

        self.elapsed_seconds += delta_seconds;
        if self.elapsed_seconds >= self.duration_seconds {
            self.finish();
        }
    }
}

#[godot_api]
impl BenchmarkSceneDirector {
    #[func]
    pub fn start(&mut self) {
        self.metrics = self.resolve_metrics();
        self.connect_metrics_source();

        if let Some(mut metrics) = self.metrics.as_ref().cloned() {
            metrics
                .bind_mut()
                .configure_refresh_interval_sec(self.refresh_interval_sec);
        }

        self.elapsed_seconds = 0.0;
        self.sample_count = 0;
        self.is_running = true;
        self.is_finished = false;
        self.stats_by_name.clear();
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
    fn _on_metrics_updated(&mut self, entries: Array<Gd<SystemPerformanceEntryDto>>) {
        if !self.is_running || self.is_finished {
            return;
        }

        self.sample_count += 1;
        for i in 0..entries.len() {
            let Some(entry_gd) = entries.get(i) else {
                continue;
            };
            let entry = entry_gd.bind();
            let name = entry.name.to_string();
            self.stats_by_name
                .entry(name)
                .or_default()
                .record(&entry);
        }
    }

    fn resolve_metrics(&mut self) -> Option<Gd<PerformanceMetrics>> {
        let host = self.base().clone().upcast::<Node>();
        PerformanceMetrics::resolve(&host)
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

        let output_path = self.global_output_path();
        let success = match self.write_report(&output_path) {
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

        self.signals()
            .benchmark_finished()
            .emit(&GString::from(output_path.to_string_lossy().as_ref()), success);

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
        PathBuf::from(project_settings.globalize_path(&self.output_file_path).to_string())
    }

    fn write_report(&self, output_path: &PathBuf) -> std::io::Result<()> {
        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent)?;
        }

        fs::write(output_path, self.build_yaml_report())
    }

    fn build_yaml_report(&self) -> String {
        let mut stats: Vec<&BenchmarkSystemStats> = self.stats_by_name.values().collect();
        stats.sort_by(|a, b| {
            b.max_avg_30s_ms
                .partial_cmp(&a.max_avg_30s_ms)
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        let mut report = String::new();
        report.push_str("benchmark:\n");
        report.push_str(&format!("  scene_id: \"{}\"\n", yaml_escape(&self.scene_id)));
        report.push_str(&format!(
            "  duration_seconds: {:.3}\n",
            self.elapsed_seconds
        ));
        report.push_str(&format!(
            "  configured_duration_seconds: {:.3}\n",
            self.duration_seconds
        ));
        report.push_str(&format!("  sample_count: {}\n", self.sample_count));
        report.push_str(&format!(
            "  refresh_interval_seconds: {:.3}\n",
            self.refresh_interval_sec
        ));
        report.push_str("systems:\n");

        for stat in stats {
            report.push_str(&format!("  - name: \"{}\"\n", yaml_escape(&stat.name)));
            report.push_str(&format!(
                "    kind: \"{}\"\n",
                if stat.is_system { "system" } else { "schedule" }
            ));
            report.push_str(&format!("    samples: {}\n", stat.samples));
            report.push_str(&format!("    calls: {}\n", stat.calls));
            report.push_str(&format!("    last_ms: {:.6}\n", stat.last_ms));
            report.push_str(&format!("    avg_ms: {:.6}\n", stat.avg_ms));
            report.push_str(&format!("    max_ms: {:.6}\n", stat.max_ms));
            report.push_str(&format!("    max_last_ms: {:.6}\n", stat.max_last_ms));
            report.push_str(&format!("    max_avg_ms: {:.6}\n", stat.max_avg_ms));
            report.push_str(&format!(
                "    max_avg_1s_ms: {:.6}\n",
                stat.max_avg_1s_ms
            ));
            report.push_str(&format!(
                "    max_avg_5s_ms: {:.6}\n",
                stat.max_avg_5s_ms
            ));
            report.push_str(&format!(
                "    max_avg_30s_ms: {:.6}\n",
                stat.max_avg_30s_ms
            ));
        }

        report
    }

    #[signal]
    fn benchmark_finished(output_file_path: GString, success: bool);
}

fn yaml_escape(value: &impl ToString) -> String {
    value.to_string().replace('\\', "\\\\").replace('"', "\\\"")
}
