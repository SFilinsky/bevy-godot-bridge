mod yaml;

use super::benchmark::BenchmarkReport;
use std::path::Path;

pub(super) use yaml::YamlBenchmarkOutputHandler;

pub(super) trait BenchmarkOutputHandler {
    fn write_report(&self, output_path: &Path, report: &BenchmarkReport) -> std::io::Result<()>;
}
