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
        self.queue.pending.borrow_mut().push(Transition {
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
            has_published: false,
            last_clock: self.queue.clock.get(),
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
    receiver: TransformPresentationReceiver,
    base: Base<RefCounted>,
}
impl TransformPresentationTrack {
    pub fn create(owner: Gd<Node>, target: Gd<Node3D>, queue: Queue<TransformValue>) -> Gd<Self> {
        Gd::from_init_fn(|base| Self {
            receiver: TransformPresentationReceiver {
                handle: Rc::new(Handle::new(owner, target, queue)),
                policy: Rc::new(Cell::new(TransformPolicy::default())),
                initialized: Rc::new(Cell::new(false)),
            },
            base,
        })
    }

    /// Retain plain native state, then release the Godot borrow before receiving values.
    pub fn native_receiver(&self) -> TransformPresentationReceiver {
        self.receiver.clone()
    }
}
#[godot_api]
impl TransformPresentationTrack {
    #[func]
    fn animate(&self, start: Transform3D, end: Transform3D, duration: f64) {
        self.receiver
            .handle
            .animate(TransformValue(start), TransformValue(end), duration);
    }
    /// Stop a transition while retaining registration for a later update.
    #[func]
    fn stop(&self) {
        self.receiver.handle.stop();
    }
    #[func]
    fn cancel(&self) {
        self.receiver.handle.token.alive.set(false);
    }

    /// Godot authors and pushes this policy when its settings change.
    #[func]
    fn configure_updates(&self, enabled: bool, duration: f64) {
        self.receiver
            .policy
            .set(TransformPolicy { enabled, duration });
    }
}

impl Drop for TransformPresentationTrack {
    fn drop(&mut self) {
        self.receiver.handle.token.alive.set(false);
    }
}

#[derive(Clone, Copy, Default)]
struct TransformPolicy {
    enabled: bool,
    duration: f64,
}

/// Typed incoming values use the authored track without a script round trip.
#[derive(Clone)]
pub struct TransformPresentationReceiver {
    handle: Rc<Handle<TransformValue>>,
    policy: Rc<Cell<TransformPolicy>>,
    initialized: Rc<Cell<bool>>,
}

impl TransformPresentationReceiver {
    /// Snap the first value; later values follow the scene's interpolation policy.
    pub fn receive(&self, previous: Transform3D, current: Transform3D) {
        if !self.handle.token.alive.get() || !self.handle.target.is_instance_valid() {
            return;
        }
        let policy = self.policy.get();
        let initial = !self.initialized.replace(true);
        // Release all Godot borrows before a scene write can call user code.
        if initial || !policy.enabled || policy.duration <= 0.0 {
            self.handle.stop();
            self.handle.target.clone().set_global_transform(current);
        } else {
            self.handle.animate(
                TransformValue(previous),
                TransformValue(current),
                policy.duration,
            );
        }
    }
}

/// One authored numeric property, using the same finite-transition policy.
#[derive(GodotClass)]
#[class(base=RefCounted, no_init)]
pub struct ScalarPresentationTrack {
    receiver: ScalarPresentationReceiver,
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
            receiver: ScalarPresentationReceiver {
                handle: Rc::new(Handle::new(owner, (target, property), queue)),
                duration: Rc::new(Cell::new(0.0)),
            },
            base,
        })
    }

    /// Keep the native destination without holding a Godot object borrow during callbacks.
    pub fn native_receiver(&self) -> ScalarPresentationReceiver {
        self.receiver.clone()
    }
}
#[godot_api]
impl ScalarPresentationTrack {
    #[func]
    fn animate(&self, start: f64, end: f64, duration: f64) {
        self.receiver
            .handle
            .animate(ScalarValue(start), ScalarValue(end), duration);
    }
    #[func]
    fn stop(&self) {
        self.receiver.handle.stop();
    }
    #[func]
    fn cancel(&self) {
        self.receiver.handle.token.alive.set(false);
    }

    /// Duration belongs to the authored visual, not its exported gameplay value.
    #[func]
    fn configure_duration(&self, duration: f64) {
        self.receiver.duration.set(duration);
    }
}

impl Drop for ScalarPresentationTrack {
    fn drop(&mut self) {
        self.receiver.handle.token.alive.set(false);
    }
}

/// Reusable destination for any exported numeric visual.
#[derive(Clone)]
pub struct ScalarPresentationReceiver {
    handle: Rc<Handle<ScalarValue>>,
    duration: Rc<Cell<f64>>,
}

impl ScalarPresentationReceiver {
    /// Continue from the displayed value, including a value changed by another scene behavior.
    pub fn animate_to(&self, end: f64) {
        let (target, property) = &self.handle.target;
        if !self.handle.token.alive.get() || !target.is_instance_valid() {
            return;
        }
        let revision = self.handle.token.revision.get();
        let start = target.get(property).to::<f64>();
        if self.handle.token.revision.get() == revision {
            self.handle
                .animate(ScalarValue(start), ScalarValue(end), self.duration.get());
        }
    }
}
