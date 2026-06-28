use super::BenchmarkOutputHandler;
use crate::performance::benchmark::{
    BenchmarkAggregateReport, BenchmarkGameplayStateReport, BenchmarkPhaseReport, BenchmarkReport,
    BenchmarkSystemReportStats,
};
use std::fs;
use std::path::Path;

pub(in crate::performance) struct YamlBenchmarkOutputHandler;

impl BenchmarkOutputHandler for YamlBenchmarkOutputHandler {
    fn write_report(&self, output_path: &Path, report: &BenchmarkReport) -> std::io::Result<()> {
        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent)?;
        }

        fs::write(output_path, build_yaml_report(report))
    }
}

fn build_yaml_report(report_data: &BenchmarkReport) -> String {
    let mut report = String::new();
    report.push_str("benchmark:\n");
    report.push_str(&format!(
        "  scene_id: \"{}\"\n",
        yaml_escape(&report_data.scene_id)
    ));
    report.push_str(&format!(
        "  duration_seconds: {:.3}\n",
        report_data.duration_seconds
    ));
    report.push_str(&format!(
        "  configured_duration_seconds: {:.3}\n",
        report_data.configured_duration_seconds
    ));
    report.push_str(&format!("  time_scale: {:.3}\n", report_data.time_scale));
    report.push_str(&format!("  frames_total: {}\n", report_data.frames_total));
    report.push_str(&format!(
        "  average_frame_ms: {:.6}\n",
        report_data.frame_summary.average_ms
    ));
    report.push_str(&format!(
        "  min_frame_ms: {:.6}\n",
        report_data.frame_summary.min_ms
    ));
    report.push_str(&format!(
        "  max_frame_ms: {:.6}\n",
        report_data.frame_summary.max_ms
    ));
    report.push_str(&format!(
        "  p50_frame_ms: {:.6}\n",
        report_data.frame_summary.p50_ms
    ));
    report.push_str(&format!(
        "  p90_frame_ms: {:.6}\n",
        report_data.frame_summary.p90_ms
    ));
    report.push_str(&format!(
        "  p95_frame_ms: {:.6}\n",
        report_data.frame_summary.p95_ms
    ));
    report.push_str(&format!(
        "  p99_frame_ms: {:.6}\n",
        report_data.frame_summary.p99_ms
    ));
    report.push_str("  frames_over_budget:\n");
    report.push_str(&format!(
        "    over_16_67_ms: {}\n",
        report_data.frames_over_budget.over_16_67_ms
    ));
    report.push_str(&format!(
        "    over_33_33_ms: {}\n",
        report_data.frames_over_budget.over_33_33_ms
    ));
    report.push_str(&format!(
        "    over_50_ms: {}\n",
        report_data.frames_over_budget.over_50_ms
    ));
    report.push_str(&format!(
        "    over_100_ms: {}\n",
        report_data.frames_over_budget.over_100_ms
    ));
    report.push_str(&format!(
        "  cpu_time_total_ms: {:.6}\n",
        report_data.cpu_time_total_ms
    ));
    report.push_str(&format!(
        "  metrics_refresh_count: {}\n",
        report_data.metrics_refresh_count
    ));
    report.push_str(&format!(
        "  refresh_interval_seconds: {:.3}\n",
        report_data.refresh_interval_seconds
    ));
    report.push_str("phases:\n");

    for phase_report in &report_data.phase_report_list {
        write_phase_report(&mut report, phase_report);
    }

    report
}

fn write_phase_report(report: &mut String, phase_report: &BenchmarkPhaseReport) {
    report.push_str(&format!(
        "  - name: \"{}\"\n",
        yaml_escape(&phase_report.phase_name)
    ));

    if !phase_report.gameplay_state_report_list.is_empty() {
        report.push_str("    gameplay_states:\n");
        for gameplay_state_report in &phase_report.gameplay_state_report_list {
            write_gameplay_state_report(report, gameplay_state_report);
        }
        return;
    }

    write_aggregate_report(report, &phase_report.aggregate, "    ");
    report.push_str("    systems:\n");

    for system in &phase_report.aggregate.system_stat_list {
        write_system_report(report, system, "      ");
    }
}

fn write_gameplay_state_report(
    report: &mut String,
    gameplay_state_report: &BenchmarkGameplayStateReport,
) {
    report.push_str(&format!(
        "      - name: \"{}\"\n",
        yaml_escape(&gameplay_state_report.phase_name)
    ));
    write_aggregate_report(report, &gameplay_state_report.aggregate, "        ");
    report.push_str("        systems:\n");

    for system in &gameplay_state_report.aggregate.system_stat_list {
        write_system_report(report, system, "          ");
    }
}

fn write_aggregate_report(report: &mut String, aggregate: &BenchmarkAggregateReport, indent: &str) {
    let nested_indent = format!("{indent}  ");
    report.push_str(&format!(
        "{indent}duration_seconds: {:.3}\n",
        aggregate.duration_seconds
    ));
    report.push_str(&format!("{indent}frames_total: {}\n", aggregate.frames_total));
    report.push_str(&format!(
        "{indent}average_frame_ms: {:.6}\n",
        aggregate.frame_summary.average_ms
    ));
    report.push_str(&format!(
        "{indent}min_frame_ms: {:.6}\n",
        aggregate.frame_summary.min_ms
    ));
    report.push_str(&format!(
        "{indent}max_frame_ms: {:.6}\n",
        aggregate.frame_summary.max_ms
    ));
    report.push_str(&format!(
        "{indent}p50_frame_ms: {:.6}\n",
        aggregate.frame_summary.p50_ms
    ));
    report.push_str(&format!(
        "{indent}p90_frame_ms: {:.6}\n",
        aggregate.frame_summary.p90_ms
    ));
    report.push_str(&format!(
        "{indent}p95_frame_ms: {:.6}\n",
        aggregate.frame_summary.p95_ms
    ));
    report.push_str(&format!(
        "{indent}p99_frame_ms: {:.6}\n",
        aggregate.frame_summary.p99_ms
    ));
    report.push_str(&format!("{indent}frames_over_budget:\n"));
    report.push_str(&format!(
        "{nested_indent}over_16_67_ms: {}\n",
        aggregate.frames_over_budget.over_16_67_ms
    ));
    report.push_str(&format!(
        "{nested_indent}over_33_33_ms: {}\n",
        aggregate.frames_over_budget.over_33_33_ms
    ));
    report.push_str(&format!(
        "{nested_indent}over_50_ms: {}\n",
        aggregate.frames_over_budget.over_50_ms
    ));
    report.push_str(&format!(
        "{nested_indent}over_100_ms: {}\n",
        aggregate.frames_over_budget.over_100_ms
    ));
    report.push_str(&format!(
        "{indent}cpu_time_total_ms: {:.6}\n",
        aggregate.cpu_time_total_ms
    ));
}

fn write_system_report(report: &mut String, system: &BenchmarkSystemReportStats, indent: &str) {
    let field_indent = format!("{indent}  ");
    report.push_str(&format!(
        "{indent}- name: \"{}\"\n",
        yaml_escape(&system.name)
    ));
    report.push_str(&format!("{field_indent}calls: {}\n", system.calls));
    report.push_str(&format!(
        "{field_indent}total_ms: {:.6}\n",
        system.summary.total_ms
    ));
    report.push_str(&format!(
        "{field_indent}average_ms: {:.6}\n",
        system.summary.average_ms
    ));
    report.push_str(&format!(
        "{field_indent}min_ms: {:.6}\n",
        system.summary.min_ms
    ));
    report.push_str(&format!(
        "{field_indent}max_ms: {:.6}\n",
        system.summary.max_ms
    ));
    report.push_str(&format!(
        "{field_indent}p50_ms: {:.6}\n",
        system.summary.p50_ms
    ));
    report.push_str(&format!(
        "{field_indent}p90_ms: {:.6}\n",
        system.summary.p90_ms
    ));
    report.push_str(&format!(
        "{field_indent}p95_ms: {:.6}\n",
        system.summary.p95_ms
    ));
    report.push_str(&format!(
        "{field_indent}p99_ms: {:.6}\n",
        system.summary.p99_ms
    ));
    report.push_str(&format!(
        "{field_indent}max_1s_average_ms: {:.6}\n",
        system.max_1s_average_ms
    ));
    report.push_str(&format!(
        "{field_indent}max_5s_average_ms: {:.6}\n",
        system.max_5s_average_ms
    ));
    report.push_str(&format!(
        "{field_indent}max_10s_average_ms: {:.6}\n",
        system.max_10s_average_ms
    ));
    report.push_str(&format!(
        "{field_indent}share_of_total_percent: {:.6}\n",
        system.share_of_total_percent
    ));
}

fn yaml_escape(value: &impl ToString) -> String {
    value.to_string().replace('\\', "\\\\").replace('"', "\\\"")
}
