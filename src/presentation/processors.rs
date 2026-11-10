//! Authored processing children; the root stores shared queues and registration state.
use super::SharedPresentation;
use godot::classes::node::ProcessMode;
use godot::classes::{INode, Node};
use godot::obj::{Base, Gd, WithBaseField};
use godot::prelude::{GodotClass, godot_api};
use std::mem::take;
observe! {
    use godot::classes::Performance;
    use godot::obj::Singleton;
}

pub(super) fn validate(root: &Gd<SharedPresentation>) {
    let mut transform_count = 0;
    let mut scalar_count = 0;
    for child in root.get_children().iter_shared() {
        if let Ok(processor) = child.clone().try_cast::<TransformPresentation>() {
            assert!(
                processor.bind().presentation.as_ref() == Some(root),
                "Transform processor must reference its Presentation root"
            );
            transform_count += 1;
        }
        if let Ok(processor) = child.try_cast::<ScalarPresentation>() {
            assert!(
                processor.bind().presentation.as_ref() == Some(root),
                "Numeric processor must reference its Presentation root"
            );
            scalar_count += 1;
        }
    }
    assert!(
        transform_count == 1 && scalar_count == 1,
        "Author one transform processor and one numeric processor under Presentation"
    );
}

/// Advances the visual clock and publishes one transform sample per display frame.
#[derive(GodotClass)]
#[class(init, base=Node)]
pub struct TransformPresentation {
    #[export]
    presentation: Option<Gd<SharedPresentation>>,
    base: Base<Node>,
}

#[godot_api]
impl INode for TransformPresentation {
    fn enter_tree(&mut self) {
        self.base_mut().set_process_mode(ProcessMode::ALWAYS);
        self.base_mut().set_physics_process_priority(i32::MAX);
        let presentation = self
            .presentation
            .as_ref()
            .expect("Assign the authored Presentation root");
        let callback = self.to_gd().callable("_publish_frame");
        self.base()
            .get_tree()
            .expect("processor is in tree")
            .connect("process_frame", &callback);
        observe! {
            let name = format!("Presentation/{}/frame publication ms", presentation.instance_id().to_i64());
            Performance::singleton().add_custom_monitor(&name, &presentation.callable("_frame_ms"));
        }
    }

    fn physics_process(&mut self, delta: f64) {
        // Time advancement does not write transforms or run gameplay.
        if let Some(presentation) = &self.presentation {
            let service = presentation.bind();
            let clock = &service.transform.queue.clock;
            clock.set(clock.get() + delta.max(0.0));
        }
    }

    fn exit_tree(&mut self) {
        let presentation = self.presentation.as_ref().expect("authored presentation");
        if let Some(mut tree) = self.base().get_tree() {
            tree.disconnect("process_frame", &self.to_gd().callable("_publish_frame"));
        }
        observe! {
            let name = format!("Presentation/{}/frame publication ms", presentation.instance_id().to_i64());
            Performance::singleton().remove_custom_monitor(&name);
        }
    }
}

#[godot_api]
impl TransformPresentation {
    #[func(gd_self)]
    fn _publish_frame(this: Gd<Self>) {
        // Release the behavior before scene writes can remove it or its level.
        let Some(mut presentation) = this.bind().presentation.clone() else {
            return;
        };
        let (mut batch, epoch) = {
            let mut service = presentation.bind_mut();
            // Repeated renders without new time or requests need no new sample.
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
        // Publish using accumulated physics time, before idle consumers read positions.
        batch.step(0.0, || {
            presentation.is_instance_valid()
                && presentation.is_inside_tree()
                && presentation.bind().epoch == epoch
        });
        // An unloaded or remounted level must not receive the old batch back.
        if presentation.is_instance_valid() && presentation.bind().epoch == epoch {
            presentation.bind_mut().transform = batch;
        }
    }
}

/// Applies finite numeric transitions through their authored visual setters.
#[derive(GodotClass)]
#[class(init, base=Node)]
pub struct ScalarPresentation {
    #[export]
    presentation: Option<Gd<SharedPresentation>>,
    base: Base<Node>,
}

#[godot_api]
impl INode for ScalarPresentation {
    fn enter_tree(&mut self) {
        self.base_mut().set_process_mode(ProcessMode::ALWAYS);
        self.base_mut().set_process_priority(i32::MAX);
        assert!(
            self.presentation.is_some(),
            "Assign the authored Presentation root"
        );
        observe! {
            let presentation = self.presentation.as_ref().expect("authored presentation");
            let name = format!("Presentation/{}/idle ms", presentation.instance_id().to_i64());
            Performance::singleton().add_custom_monitor(&name, &presentation.callable("_idle_ms"));
        }
    }

    #[func(gd_self)]
    fn process(this: Gd<Self>, delta: f64) {
        // A property setter can unload this scene; release the processor first.
        let presentation = this.bind().presentation.clone();
        let Some(mut presentation) = presentation else {
            return;
        };
        let (mut batch, epoch) = {
            let mut service = presentation.bind_mut();
            let batch = take(&mut service.scalar);
            service.scalar.queue = batch.queue.clone();
            (batch, service.epoch)
        };
        // Advance and apply while neither authored node is borrowed.
        batch.step(delta, || {
            presentation.is_instance_valid()
                && presentation.is_inside_tree()
                && presentation.bind().epoch == epoch
        });
        if presentation.is_instance_valid() && presentation.bind().epoch == epoch {
            presentation.bind_mut().scalar = batch;
        }
    }

    fn exit_tree(&mut self) {
        observe! {
            let presentation = self.presentation.as_ref().expect("authored presentation");
            let name = format!("Presentation/{}/idle ms", presentation.instance_id().to_i64());
            Performance::singleton().remove_custom_monitor(&name);
        }
    }
}
