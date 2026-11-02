//! Condenses repeated calls while keeping the first occurrence of each stage in time order.
use super::startup::{Kind, Recording};
use super::startup_memory::Memory;
use std::collections::{BTreeMap, HashSet};
use std::thread::ThreadId;

#[derive(Default)]
pub(crate) struct Row {
    pub name: String,
    pub within: Option<String>,
    pub first_ms: f64,
    pub end_ms: f64,
    pub work_ms: f64,
    pub calls: usize,
}

pub(crate) struct Summary {
    pub origin: &'static str,
    pub endpoint: Option<&'static str>,
    pub memory: Option<Memory>,
    pub first_action_ms: Option<f64>,
    pub dropped: usize,
    pub threads: usize,
    pub stages: Vec<Row>,
    pub contributors: Vec<Row>,
    pub last_ms: f64,
}

impl Summary {
    pub fn from_recording(recording: &Recording) -> Self {
        let mut stages = BTreeMap::<String, Row>::new();
        let mut details = BTreeMap::<(String, String), Row>::new();
        let owners: Vec<_> = recording.samples.iter().filter(|s| s.kind == Kind::Stage).collect();
        let mut threads = HashSet::<ThreadId>::new();
        let mut last_ms = 0.0_f64;
        // Fold intervals independently: parent and child times overlap.
        for sample in &recording.samples {
            let at = sample.at.as_secs_f64() * 1000.0;
            let duration = sample.duration.as_secs_f64() * 1000.0;
            threads.insert(sample.thread);
            last_ms = last_ms.max(at + duration);
            let row = if matches!(sample.kind, Kind::Stage | Kind::Marker) {
                stages.entry(sample.name.clone()).or_insert_with(|| Row {
                    name: sample.name.clone(), first_ms: at, ..Row::default()
                })
            } else {
                // A worker system belongs to its app's enclosing update. For other
                // details only claim a containing stage on the same thread.
                let owner = owners.iter().filter(|s| s.at <= sample.at
                    && s.at + s.duration >= sample.at + sample.duration
                    && (s.thread == sample.thread || (matches!(sample.kind, Kind::System | Kind::Schedule) && s.name == "Bevy update")))
                    .min_by_key(|s| s.duration).map(|s| s.name.as_str()).unwrap_or("between stages");
                let prefix = match sample.kind { Kind::System => "system: ", Kind::Schedule => "schedule: ", _ => "" };
                let name = format!("{prefix}{}", sample.name);
                details.entry((name.clone(), owner.into())).or_insert_with(|| Row {
                    name, within: Some(owner.into()), first_ms: at, ..Row::default()
                })
            };
            row.first_ms = row.first_ms.min(at);
            row.end_ms = row.end_ms.max(at + duration);
            row.work_ms += duration;
            row.calls += 1;
        }
        let mut stages: Vec<_> = stages.into_values().collect();
        stages.sort_by(|a,b| a.first_ms.total_cmp(&b.first_ms));
        let mut contributors: Vec<_> = details.into_values().collect();
        contributors.sort_by(|a,b| b.work_ms.total_cmp(&a.work_ms));
        contributors.truncate(20);
        Self { origin: recording.origin_name, endpoint: recording.endpoint, memory: recording.memory,
            first_action_ms: recording.first_action.map(|t| t.as_secs_f64() * 1000.0),
            dropped: recording.dropped, threads: threads.len(), stages, contributors, last_ms }
    }
}
