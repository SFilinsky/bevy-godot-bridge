use super::dto::{PerformanceMetrics, SystemPerformanceEntryDto};
use godot::classes::{Control, INode, Label, Node};
use godot::obj::{Base, Gd, NewAlloc};
use godot::prelude::*;

/// Godot UI node that renders Bevy system performance stats.
///
/// Intended usage:
/// - Add as a child somewhere in your debug UI scene
/// - Ensure `bevy_app_path` points at your autoload singleton (default: `/root/BevyAppSingleton`)
/// - Assign `container_path` in the editor to any Control that should host the labels
///
/// This Node will pull metrics and render labels as children of the referenced container.
/// It uses typed arrays on the Godot side.
///
/// Display logic:
/// - Schedules are displayed first in hardcoded order. If missing, prints 0 values.
/// - Systems are displayed below, sorted by avg_5s_ms descending.
#[derive(GodotClass)]
#[class(base = Node)]
pub struct PerformanceHud {
    /// Path to BevyApp singleton autoload.
    /// Default assumes you have an autoload named `BevyAppSingleton`.
    #[export]
    bevy_app_path: NodePath,

    /// Path (relative to this node) to a Node that will host the labels.
    /// This is configured in the editor by the consumer.
    #[export]
    container_path: NodePath,

    /// Refresh interval in seconds. (e.g. 0.25 = 4 times/sec)
    #[export]
    refresh_interval_sec: f64,

    /// Optional max number of rows to display (0 = unlimited).
    #[export]
    max_rows: i32,

    metrics: Option<Gd<PerformanceMetrics>>,

    #[var]
    container: Option<Gd<Control>>,

    labels: Vec<Gd<Label>>,
    time_accum: f64,

    #[base]
    base: Base<Node>,
}

#[godot_api]
impl INode for PerformanceHud {
    fn init(base: Base<Node>) -> Self {
        Self {
            bevy_app_path: NodePath::from("/root/BevyAppSingleton"),
            container_path: NodePath::from(""),
            refresh_interval_sec: 0.25,
            max_rows: 60,
            metrics: None,
            container: None,
            labels: Vec::new(),
            time_accum: 0.0,
            base,
        }
    }

    fn ready(&mut self) {
        self.container = self.resolve_container();
        self.metrics = self.resolve_metrics();
        self.refresh();
    }

    fn process(&mut self, delta: f64) {
        self.time_accum += delta;

        if self.refresh_interval_sec <= 0.0 {
            return;
        }
        if self.time_accum < self.refresh_interval_sec {
            return;
        }
        self.time_accum = 0.0;

        self.refresh_now();
    }
}

#[godot_api]
impl PerformanceHud {
    #[func]
    pub fn refresh_now(&mut self) {
        if self.container.is_none() {
            self.container = self.resolve_container();
        }
        if self.metrics.is_none() {
            self.metrics = self.resolve_metrics();
        }
        self.refresh();
    }

    fn resolve_container(&mut self) -> Option<Gd<Control>> {
        if self.container_path.is_empty() {
            return None;
        }
        let container_node = self.base().get_node_or_null(&self.container_path)?;
        container_node.try_cast::<Control>().ok()
    }

    fn resolve_metrics(&mut self) -> Option<Gd<PerformanceMetrics>> {
        let tree = self.base().get_tree()?;
        let root = tree.get_root();

        let bevy_root: Gd<Node> = root.unwrap().get_node_or_null(&self.bevy_app_path)?;

        let child_count = bevy_root.get_child_count();
        for i in 0..child_count {
            let child = bevy_root.get_child(i)?;
            if let Ok(metrics) = child.try_cast::<PerformanceMetrics>() {
                return Some(metrics);
            }
        }

        None
    }

    fn refresh(&mut self) {
        let Some(mut container) = self.container.clone() else {
            self.show_single_line("PerformanceHud: container_path not set or invalid");
            return;
        };

        let Some(metrics) = self.metrics.as_ref() else {
            self.show_single_line(
                "PerformanceHud: metrics unavailable (BevyAppSingleton not found?)",
            );
            return;
        };

        let entries = metrics.bind().get_metrics();

        // Better schedule order based on the Bevy excerpt you pasted.
        // (Some items might not exist depending on enabled features/plugins; we show 0 in that case.)
        const SCHEDULE_ORDER: &[&str] = &[
            // Sometimes shows up
            "Main",
            // Startup phase
            "PreStartup",
            "Startup",
            "PostStartup",
            // Main loop phase
            "First",
            "PreUpdate",
            "StateTransition",
            "RunFixedMainLoop",
            // Fixed main breakdown (may appear depending on span naming)
            "FixedMain",
            "FixedFirst",
            "FixedPreUpdate",
            "FixedUpdate",
            "FixedPostUpdate",
            "FixedLast",
            // Back to variable update
            "Update",
            "SpawnScene",
            "PostUpdate",
            "Last",
        ];

        // schedules[name] -> (a1, a5, a30, calls)
        let mut schedules: std::collections::HashMap<String, (f64, f64, f64, i64)> =
            std::collections::HashMap::new();

        // systems: (name, a1, a5, a30, calls)
        let mut systems: Vec<(String, f64, f64, f64, i64)> = Vec::new();

        for i in 0..entries.len() {
            let entry_gd: Gd<SystemPerformanceEntryDto> = entries.get(i).unwrap();
            let entry = entry_gd.bind();

            let name = entry.name.to_string();
            let a1 = entry.avg_1s_ms;
            let a5 = entry.avg_5s_ms;
            let a30 = entry.avg_30s_ms;
            let calls = entry.calls;

            if entry.is_system {
                systems.push((name, a1, a5, a30, calls));
            } else {
                schedules.insert(name, (a1, a5, a30, calls));
            }
        }

        // Systems sorted by avg_5s descending (stable + responsive)
        systems.sort_by(|a, b| b.2.partial_cmp(&a.2).unwrap_or(std::cmp::Ordering::Equal));

        let mut lines: Vec<String> = Vec::new();

        // Schedules first (fixed order, show 0 if missing)
        for sched in SCHEDULE_ORDER {
            if let Some((a1, a5, a30, calls)) = schedules.get(*sched) {
                lines.push(format!(
                    "{:<22} a1 {:>7.3}  a5 {:>7.3}  a30 {:>7.3} ms   calls {}",
                    *sched, *a1, *a5, *a30, *calls
                ));
            } else {
                lines.push(format!(
                    "{:<22} a1 {:>7.3}  a5 {:>7.3}  a30 {:>7.3} ms   calls {}",
                    *sched, 0.0, 0.0, 0.0, 0
                ));
            }
        }

        // Systems below schedules
        for (name, a1, a5, a30, calls) in systems.iter() {
            lines.push(format!(
                "{:<22} a1 {:>7.3}  a5 {:>7.3}  a30 {:>7.3} ms   calls {}",
                name, *a1, *a5, *a30, *calls
            ));
        }

        // Respect max_rows (0 = unlimited)
        let mut count = lines.len() as i32;
        if self.max_rows > 0 {
            count = count.min(self.max_rows);
        }

        self.ensure_label_count(&mut container, count as usize);

        for i in 0..(count as usize) {
            let text = &lines[i];
            let mut label = self.labels[i].clone();
            label.set_text(text);
            label.set_visible(true);
        }

        for i in (count as usize)..self.labels.len() {
            self.labels[i].set_visible(false);
        }
    }

    fn show_single_line(&mut self, msg: &str) {
        let Some(mut container) = self.container.clone() else {
            return;
        };

        self.ensure_label_count(&mut container, 1);
        let mut label = self.labels[0].clone();
        label.set_text(msg);
        label.set_visible(true);

        for i in 1..self.labels.len() {
            self.labels[i].set_visible(false);
        }
    }

    fn ensure_label_count(&mut self, container: &mut Gd<Control>, desired: usize) {
        while self.labels.len() < desired {
            let mut label = Label::new_alloc();
            label.set_text("");
            label.set_autowrap_mode(godot::classes::text_server::AutowrapMode::OFF);
            label.set_clip_text(true);

            container.add_child(&label);
            self.labels.push(label);
        }
    }
}
