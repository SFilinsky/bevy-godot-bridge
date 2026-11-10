//! Animate changed values in shared native loops while scenes keep their settings.
//!
//! Author one complete presentation scene below the level's RootScripts.
//! Behaviors register a target once, submit finite transitions, and cancel on exit.
//! Transforms use physics time and publish once before idle consumers run.
//! Numeric properties run once in idle processing.

mod batch;
mod processors;
mod tracks;
observe! { mod probe; }

use crate::scene::registration::{register_level_service_on_entry, scene_services};
use crate::scene::scene_root::SceneRoot;
use batch::{Batch, ScalarValue, TransformValue};
use godot::builtin::{StringName, VarDictionary};
use godot::classes::{INode, Node, Node3D};
use godot::global::godot_error;
use godot::obj::{Base, Gd, WithBaseField};
use godot::prelude::{GodotClass, godot_api};
use processors::validate as validate_processors;
use std::mem::take;
pub use tracks::{ScalarPresentationTrack, TransformPresentationTrack};
observe! {
    use probe::target_for;
}

/// Scene-authored service for finite visual transitions, independent of Bevy updates.
#[derive(GodotClass)]
#[class(base=Node)]
pub struct SharedPresentation {
    transform: Batch<TransformValue>,
    scalar: Batch<ScalarValue>,
    last_frame_clock: f64,
    epoch: u64,
    base: Base<Node>,
}

#[godot_api]
impl SharedPresentation {
    /// Resolve the explicitly authored root service without searching entity children.
    #[func]
    fn resolve(host: Gd<Node>) -> Option<Gd<Self>> {
        let root = SceneRoot::resolve_as_parent(&host)?;
        let mut services = scene_services::<Self>(&root, &host);
        if services.len() != 1 {
            godot_error!(
                "Presentation: author exactly one complete presentation scene in this level"
            );
            return None;
        }
        services.pop()
    }

    /// Register a visual destination, published once per displayed frame using physics time.
    #[func]
    fn transform_track(
        &mut self,
        owner: Gd<Node>,
        target: Gd<Node3D>,
    ) -> Gd<TransformPresentationTrack> {
        observe! { let target = target_for(owner.clone(), target); }
        TransformPresentationTrack::create(owner, target, self.transform.queue.clone())
    }

    /// Register a numeric property; its setter remains responsible for the visual.
    #[func]
    fn scalar_track(
        &mut self,
        owner: Gd<Node>,
        target: Gd<Node>,
        property: StringName,
    ) -> Gd<ScalarPresentationTrack> {
        ScalarPresentationTrack::create(owner, target, property, self.scalar.queue.clone())
    }

    #[func]
    fn reset_measurements(&mut self) {
        self.transform.metrics = Default::default();
        self.scalar.metrics = Default::default();
    }

    #[func]
    fn measurements(&self) -> VarDictionary {
        let mut result = VarDictionary::new();
        result.set("frame", self.transform.metrics.dictionary());
        result.set("idle", self.scalar.metrics.dictionary());
        result
    }

    #[func]
    fn _frame_ms(&self) -> f64 {
        self.transform.metrics.last_ms
    }
    #[func]
    fn _idle_ms(&self) -> f64 {
        self.scalar.metrics.last_ms
    }

    #[func(gd_self)]
    fn _publish_frame(mut this: Gd<Self>) {
        // Publish once before idle consumers read positions, without advancing physics time again.
        let (mut batch, epoch) = {
            let mut service = this.bind_mut();
            // Fast displays may render repeatedly without a physics step or new request.
            let clock = service.transform.queue.clock.get();
            if clock == service.last_frame_clock
                && service.transform.queue.pending.borrow().is_empty()
            {
                return;
            }
            service.last_frame_clock = clock;
            let batch = take(&mut service.transform);
            service.transform.queue = batch.queue.clone();
            (batch, service.epoch)
        };
        batch.step(0.0, || {
            this.is_instance_valid() && this.is_inside_tree() && this.bind().epoch == epoch
        });
        if this.is_instance_valid() && this.bind().epoch == epoch {
            this.bind_mut().transform = batch;
        }
    }
}

#[godot_api]
impl INode for SharedPresentation {
    fn init(base: Base<Node>) -> Self {
        register_level_service_on_entry(&base);
        Self {
            transform: Batch::default(),
            scalar: Batch::default(),
            last_frame_clock: 0.0,
            epoch: 0,
            base,
        }
    }

    fn exit_tree(&mut self) {
        self.epoch += 1;
        self.transform.clear();
        self.scalar.clear();
    }
    fn ready(&mut self) {
        validate_processors(&self.to_gd());
    }
}

impl SharedPresentation {
    pub(super) fn advance_clock(&self, delta: f64) {
        let clock = &self.transform.queue.clock;
        clock.set(clock.get() + delta.max(0.0));
    }

    pub(super) fn advance_numbers(mut this: Gd<Self>, delta: f64) {
        let (mut batch, epoch) = {
            let mut service = this.bind_mut();
            let batch = take(&mut service.scalar);
            service.scalar.queue = batch.queue.clone();
            (batch, service.epoch)
        };
        batch.step(delta, || {
            this.is_instance_valid() && this.is_inside_tree() && this.bind().epoch == epoch
        });
        if this.is_instance_valid() && this.bind().epoch == epoch {
            this.bind_mut().scalar = batch;
        }
    }
}
