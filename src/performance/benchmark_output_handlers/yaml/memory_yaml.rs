use crate::performance::memory::{CounterStats, INTERVAL, MemoryReport};
use std::fmt::Write;

/// Write one compact block. Raw process readings stay in memory.
pub(super) fn write_memory(out: &mut String, report: &MemoryReport) {
    let _ = writeln!(
        out,
        "memory:\n  scope: game_process\n  interval_seconds: {}\n  periodic_sampling: {}",
        INTERVAL.as_secs(),
        report.periodic
    );
    for (name, period) in [
        ("whole_run", &report.whole_run),
        ("loading", &report.loading),
        ("gameplay", &report.gameplay),
    ] {
        if period.count == 0 {
            continue;
        }
        let _ = writeln!(
            out,
            "  {name}:\n    samples: {}\n    elapsed_seconds: {:.6}",
            period.count,
            period.duration.as_secs_f64()
        );
        write_counter(out, "resident", &period.resident);
        write_counter(out, "private_commit", &period.private_commit);
    }
}

fn write_counter(out: &mut String, name: &str, stats: &CounterStats) {
    let Some((peak, at)) = stats.peak else { return };
    let _ = writeln!(
        out,
        "    {name}:\n      valid_samples: {}\n      covered_seconds: {:.6}",
        stats.samples,
        stats.covered.as_secs_f64()
    );
    for (label, reading) in [
        ("start", stats.start),
        ("end", stats.end),
        ("avg", stats.average()),
        ("p50", stats.percentile(50)),
        ("p95", stats.percentile(95)),
    ] {
        if let Some(bytes) = reading {
            let _ = writeln!(out, "      {label}_bytes: {bytes}");
        }
    }
    let _ = writeln!(
        out,
        "      peak_bytes: {peak}\n      peak_at_seconds: {:.6}",
        at.as_secs_f64()
    );
    if let (Some(start), Some(end)) = (stats.start, stats.end) {
        let _ = writeln!(out, "      change_bytes: {}", end as i128 - start as i128);
    }
}
