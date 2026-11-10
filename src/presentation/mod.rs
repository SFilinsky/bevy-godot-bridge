//! Animate changed values in shared native loops while scenes keep their settings.
//!
//! Author one complete presentation scene below the level's RootScripts.
//! Behaviors register a target once, submit finite transitions, and cancel on exit.
//! Transform and numeric processors advance their finite transitions independently.
//! Numeric properties run once in idle processing.

mod batch;
mod processors;
mod tracks;

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

/// Scene-authored service for finite visual transitions, independent of Bevy updates.
#[derive(GodotClass)]
#[class(base=Node)]
pub struct SharedPresentation {
    transform: Batch<TransformValue>,
    scalar: Batch<ScalarValue>,
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

    /// Register a visual destination, advanced by the authored transform processor.
    #[func]
    fn transform_track(
        &mut self,
        owner: Gd<Node>,
        target: Gd<Node3D>,
    ) -> Gd<TransformPresentationTrack> {
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
        result.set("physics", self.transform.metrics.dictionary());
        result.set("idle", self.scalar.metrics.dictionary());
        result
    }

    #[func]
    fn _physics_ms(&self) -> f64 {
        self.transform.metrics.last_ms
    }
    #[func]
    fn _idle_ms(&self) -> f64 {
        self.scalar.metrics.last_ms
    }
}

#[godot_api]
impl INode for SharedPresentation {
    fn init(base: Base<Node>) -> Self {
        register_level_service_on_entry(&base);
        Self {
            transform: Batch::default(),
            scalar: Batch::default(),
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
    pub(super) fn advance_transforms(mut this: Gd<Self>, delta: f64) {
        // Release the service borrow before writes can invoke scripts or detach scenes.
        let (mut batch, epoch) = {
            let mut service = this.bind_mut();
            let batch = take(&mut service.transform);
            service.transform.queue = batch.queue.clone();
            (batch, service.epoch)
        };
        batch.step(delta, || {
            this.is_instance_valid() && this.is_inside_tree() && this.bind().epoch == epoch
        });
        if this.is_instance_valid() && this.bind().epoch == epoch {
            this.bind_mut().transform = batch;
        }
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
