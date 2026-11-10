//! Retained scene handles submit plain values, without creating an animation object.
use super::batch::{Queue, ScalarValue, Token, TransformValue, Transition, Value};
use godot::builtin::{StringName, Transform3D};
use godot::classes::{Node, Node3D, RefCounted};
use godot::obj::{Base, Gd};
use godot::prelude::{GodotClass, godot_api};
use std::cell::Cell;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

static NEXT_ID: AtomicU64 = AtomicU64::new(1);

struct Handle<T: Value> {
    id: u64,
    owner: Gd<Node>,
    target: T::Target,
    token: Rc<Token>,
    queue: Queue<T>,
}

impl<T: Value> Handle<T> {
    fn new(owner: Gd<Node>, target: T::Target, queue: Queue<T>) -> Self {
        Self {
            id: NEXT_ID.fetch_add(1, Ordering::Relaxed),
            owner,
            target,
            token: Rc::new(Token {
                revision: Cell::new(0),
                alive: Cell::new(true),
            }),
            queue,
        }
    }
    fn animate(&self, start: T, end: T, duration: f64) {
        if !self.token.alive.get() || !duration.is_finite() {
            return;
        }
        self.stop();
        self.queue.borrow_mut().push(Transition {
            id: self.id,
            token: self.token.clone(),
            revision: self.token.revision.get(),
            owner: self.owner.clone(),
            target: self.target.clone(),
            start,
            end,
            duration: duration.max(0.0),
            elapsed: 0.0,
            completed: false,
        });
    }
    fn stop(&self) {
        self.token
            .revision
            .set(self.token.revision.get().wrapping_add(1));
    }
}
impl<T: Value> Drop for Handle<T> {
    fn drop(&mut self) {
        self.token.alive.set(false);
    }
}

/// One authored transform destination, with no per-update Tween or callable.
#[derive(GodotClass)]
#[class(base=RefCounted, no_init)]
pub struct TransformPresentationTrack {
    handle: Handle<TransformValue>,
    base: Base<RefCounted>,
}
impl TransformPresentationTrack {
    pub fn create(owner: Gd<Node>, target: Gd<Node3D>, queue: Queue<TransformValue>) -> Gd<Self> {
        Gd::from_init_fn(|base| Self {
            handle: Handle::new(owner, target, queue),
            base,
        })
    }
}
#[godot_api]
impl TransformPresentationTrack {
    #[func]
    fn animate(&self, start: Transform3D, end: Transform3D, duration: f64) {
        self.handle
            .animate(TransformValue(start), TransformValue(end), duration);
    }
    /// Stop a transition while retaining registration for a later update.
    #[func]
    fn stop(&self) {
        self.handle.stop();
    }
    #[func]
    fn cancel(&self) {
        self.handle.token.alive.set(false);
    }
}

/// One authored numeric property, using the same finite-transition policy.
#[derive(GodotClass)]
#[class(base=RefCounted, no_init)]
pub struct ScalarPresentationTrack {
    handle: Handle<ScalarValue>,
    base: Base<RefCounted>,
}
impl ScalarPresentationTrack {
    pub fn create(
        owner: Gd<Node>,
        target: Gd<Node>,
        property: StringName,
        queue: Queue<ScalarValue>,
    ) -> Gd<Self> {
        Gd::from_init_fn(|base| Self {
            handle: Handle::new(owner, (target, property), queue),
            base,
        })
    }
}
#[godot_api]
impl ScalarPresentationTrack {
    #[func]
    fn animate(&self, start: f64, end: f64, duration: f64) {
        self.handle
            .animate(ScalarValue(start), ScalarValue(end), duration);
    }
    #[func]
    fn stop(&self) {
        self.handle.stop();
    }
    #[func]
    fn cancel(&self) {
        self.handle.token.alive.set(false);
    }
}
