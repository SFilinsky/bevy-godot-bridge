//! Compact active transitions and typed output buffers shared by visual policies.
use godot::builtin::{StringName, Transform3D, VarDictionary};
use godot::classes::{Node, Node3D};
use godot::obj::Gd;
use godot::prelude::ToGodot;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
observe! {
    use std::time::Instant;
    use super::probe::writes_enabled;
}

pub trait Value: Copy {
    type Target: Clone;
    fn interpolate(start: Self, end: Self, weight: f64) -> Self;
    fn valid(target: &Self::Target) -> bool;
    fn apply(self, target: &mut Self::Target);
}

#[derive(Clone, Copy)]
pub struct TransformValue(pub Transform3D);
impl Value for TransformValue {
    type Target = Gd<Node3D>;
    fn interpolate(start: Self, end: Self, weight: f64) -> Self {
        Self(start.0.interpolate_with(&end.0, weight as f32))
    }
    fn valid(target: &Self::Target) -> bool {
        target.is_instance_valid() && target.is_inside_tree()
    }
    fn apply(self, target: &mut Self::Target) {
        target.set_global_transform(self.0);
    }
}

#[derive(Clone, Copy)]
pub struct ScalarValue(pub f64);
impl Value for ScalarValue {
    type Target = (Gd<Node>, StringName);
    fn interpolate(start: Self, end: Self, weight: f64) -> Self {
        Self(start.0 + (end.0 - start.0) * weight)
    }
    fn valid(target: &Self::Target) -> bool {
        target.0.is_instance_valid() && target.0.is_inside_tree()
    }
    fn apply(self, target: &mut Self::Target) {
        target.0.set(&target.1, &self.0.to_variant());
    }
}

/// Shared version invalidates old work immediately, even inside a property setter.
pub struct Token {
    pub revision: Cell<u64>,
    pub alive: Cell<bool>,
}

pub struct Transition<T: Value> {
    pub id: u64,
    pub token: Rc<Token>,
    pub revision: u64,
    pub owner: Gd<Node>,
    pub target: T::Target,
    pub start: T,
    pub end: T,
    pub duration: f64,
    pub elapsed: f64,
    pub completed: bool,
    pub has_published: bool,
    pub last_clock: f64,
}

impl<T: Value> Transition<T> {
    fn valid(&self) -> bool {
        self.token.alive.get()
            && self.token.revision.get() == self.revision
            && self.owner.is_instance_valid()
            && self.owner.is_inside_tree()
            && T::valid(&self.target)
    }
}

/// Shared clock lets render publication combine several physics steps precisely.
pub struct Inbox<T: Value> {
    pub pending: RefCell<Vec<Transition<T>>>,
    pub clock: Cell<f64>,
}

impl<T: Value> Default for Inbox<T> {
    fn default() -> Self {
        Self {
            pending: RefCell::new(Vec::new()),
            clock: Cell::new(0.0),
        }
    }
}

pub type Queue<T> = Rc<Inbox<T>>;

#[derive(Default)]
pub struct Metrics {
    pub last_ms: f64,
    prepare_ms: f64,
    write_ms: f64,
    calls: u64,
    writes: u64,
    evaluations: u64,
}

impl Metrics {
    pub fn dictionary(&self) -> VarDictionary {
        let mut result = VarDictionary::new();
        result.set("prepare_ms", self.prepare_ms);
        result.set("write_ms", self.write_ms);
        result.set("calls", self.calls);
        result.set("writes", self.writes);
        result.set("evaluations", self.evaluations);
        result
    }
}

pub struct Batch<T: Value> {
    pub queue: Queue<T>,
    active_list: Vec<Transition<T>>,
    index_map: HashMap<u64, usize>,
    output_list: Vec<(usize, T)>,
    pub metrics: Metrics,
}

impl<T: Value> Default for Batch<T> {
    fn default() -> Self {
        Self {
            queue: Rc::default(),
            active_list: Vec::new(),
            index_map: HashMap::new(),
            output_list: Vec::new(),
            metrics: Metrics::default(),
        }
    }
}

impl<T: Value> Batch<T> {
    pub fn clear(&mut self) {
        for entry in self.active_list.drain(..) {
            entry.token.alive.set(false);
        }
        for entry in self.queue.pending.borrow_mut().drain(..) {
            entry.token.alive.set(false);
        }
        self.index_map.clear();
        self.output_list.clear();
    }

    pub fn step(&mut self, delta: f64, service_valid: impl Fn() -> bool) {
        #[cfg(not(any(
            all(debug_assertions, feature = "dev-observability"),
            feature = "profiled-release"
        )))]
        let apply_writes = true;
        observe! { let apply_writes = writes_enabled(); }
        observe! { let started = Instant::now(); }
        self.queue
            .clock
            .set(self.queue.clock.get() + delta.max(0.0));
        let clock = self.queue.clock.get();
        // Replace only changed transitions; finished destinations leave the hot loop.
        for entry in self.queue.pending.borrow_mut().drain(..) {
            if !entry.valid() {
                continue;
            }
            if let Some(&index) = self.index_map.get(&entry.id) {
                self.active_list[index] = entry;
            } else {
                self.index_map.insert(entry.id, self.active_list.len());
                self.active_list.push(entry);
            }
        }
        self.output_list.clear();
        for (index, entry) in self.active_list.iter_mut().enumerate() {
            // A new request starts at submission, even between catch-up physics steps.
            let elapsed_delta = (clock - entry.last_clock).max(0.0);
            entry.last_clock = clock;
            if !entry.valid() || !entry.owner.can_process() {
                continue;
            }
            // A replacement must publish its start even before another physics tick.
            if elapsed_delta == 0.0 && entry.duration > 0.0 && entry.has_published {
                continue;
            }
            entry.elapsed = (entry.elapsed + elapsed_delta).min(entry.duration);
            let weight = if entry.duration > 0.0 {
                entry.elapsed / entry.duration
            } else {
                1.0
            };
            self.output_list
                .push((index, T::interpolate(entry.start, entry.end, weight)));
        }
        observe! { let prepared = Instant::now(); }
        // Native destinations may call scripts; check cancellation again before each write.
        for &(index, value) in &self.output_list {
            if !service_valid() {
                break;
            }
            let entry = &mut self.active_list[index];
            if entry.valid() && entry.owner.can_process() {
                if apply_writes {
                    value.apply(&mut entry.target);
                    observe! { self.metrics.writes += 1; }
                }
                entry.completed = entry.elapsed >= entry.duration;
                entry.has_published = true;
                observe! { self.metrics.evaluations += 1; }
            }
        }
        // Swap removal keeps the remaining jobs compact and their lookup indices correct.
        let mut index = 0;
        while index < self.active_list.len() {
            let entry = &self.active_list[index];
            if !entry.valid() || entry.completed {
                self.index_map.remove(&entry.id);
                self.active_list.swap_remove(index);
                if let Some(moved) = self.active_list.get(index) {
                    self.index_map.insert(moved.id, index);
                }
            } else {
                index += 1;
            }
        }
        observe! {
            let finished = Instant::now();
            self.metrics.prepare_ms += (prepared - started).as_secs_f64() * 1000.0;
            self.metrics.write_ms += (finished - prepared).as_secs_f64() * 1000.0;
            self.metrics.last_ms = (finished - started).as_secs_f64() * 1000.0;
            self.metrics.calls += 1;
        }
    }
}
