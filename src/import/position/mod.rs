pub mod intentions {
    use bevy::prelude::{Message, Vec3};

    /// Request to set/update position of an existing Bevy entity identified by `godot_id`.
    #[derive(Message, Debug, Clone, Copy)]
    pub struct InitializePositionIntention {
        pub godot_id: i64,
        pub position: Vec3,
    }
}

mod systems {
    use super::intentions::InitializePositionIntention;
    use bevy::prelude::*;
    use bevy_godot4::prelude::GodotEntity;
    use godot::global::{godot_error, godot_print};
    use once_cell::sync::Lazy;
    use std::collections::VecDeque;
    use std::sync::Mutex;

    static POSITION_QUEUE: Lazy<Mutex<VecDeque<InitializePositionIntention>>> =
        Lazy::new(|| Mutex::new(VecDeque::new()));

    /// Called from the Godot-facing importer (in the same process).
    pub fn enqueue_position(godot_id: i64, position: Vec3) {
        POSITION_QUEUE
            .lock()
            .unwrap()
            .push_back(InitializePositionIntention { godot_id, position });
    }

    /// Drain local queue into Bevy's message channel.
    pub(super) fn drain_intentions(mut out: MessageWriter<InitializePositionIntention>) {
        let mut q = POSITION_QUEUE.lock().unwrap();
        for i in q.drain(..) {
            out.write(i);
        }
    }

    pub(super) fn log_intentions(mut ev: MessageReader<InitializePositionIntention>) {
        for InitializePositionIntention { godot_id, position } in ev.read().copied() {
            godot_print!(
                "[PositionInit] request godot_id={} at ({:.2},{:.2},{:.2})",
                godot_id,
                position.x,
                position.y,
                position.z
            );
        }
    }

    /// Apply position to an already-spawned Bevy entity that has the matching GodotEntity id.
    ///
    /// This system sets the entity's Transform translation. If Transform is missing, it inserts it.
    /// (This keeps EntityInitialization generic and does not force Transform on all entities.)
    pub(super) fn handle_initialize_position(
        mut ev: MessageReader<InitializePositionIntention>,
        q: Query<(Entity, &GodotEntity, Option<&Transform>)>,
        mut cmd: Commands,
    ) {
        for InitializePositionIntention { godot_id, position } in ev.read().copied() {
            let Some((entity, _, maybe_t)) = q.iter().find(|(_, ge, _)| ge.godot_id == godot_id)
            else {
                godot_error!(
                    "Trying to set position for godot_id={} but no matching Bevy entity found. \
                     Make sure EntityImporter initialized it before PositionImporter runs.",
                    godot_id
                );
                continue;
            };

            match maybe_t {
                Some(t) => {
                    let mut t2 = *t;
                    t2.translation = position;
                    cmd.entity(entity).insert(t2);
                }
                None => {
                    cmd.entity(entity)
                        .insert(Transform::from_translation(position));
                }
            }
        }
    }
}

pub mod importers {
    use super::systems::enqueue_position;
    use crate::prelude::EntityImporter;
    use bevy::prelude::Vec3;
    use godot::classes::{Node, Node3D};
    use godot::global::{godot_error, godot_print};
    use godot::obj::Base;
    use godot::prelude::*;

    /// Godot-facing node that sends a position-init request to Bevy.
    ///
    /// - Attach as a CHILD of EntityImporter (recommended; simplest).
    /// - Set `target_node` to any Node3D whose position you want to read.
    ///
    /// On `_ready()`:
    /// - Reads parent EntityImporter.entity_id
    /// - Reads target_node.global_transform.origin
    /// - Enqueues InitializePositionIntention
    #[derive(GodotClass)]
    #[class(init, base=Node)]
    pub struct PositionImporter {
        /// REQUIRED: Node3D to read global position from.
        #[export]
        #[var]
        target_node: Option<Gd<Node3D>>,

        #[base]
        base: Base<Node>,
    }

    #[godot_api]
    impl PositionImporter {
        fn read_parent_entity_id(&self) -> Option<i64> {
            let parent = self.base().get_parent();
            let importer = parent?.try_cast::<EntityImporter>().ok()?;
            Some(importer.bind().get_entity_id())
        }

        fn register_internal(&self) {
            let Some(godot_id) = self.read_parent_entity_id() else {
                let path = self.base().get_path();
                godot_error!("PositionImporter at '{path}' must be a child of EntityImporter");
                return;
            };

            if godot_id <= 0 {
                let path = self.base().get_path();
                godot_error!(
                    "PositionImporter at '{path}' couldn't read a valid 'entity_id' from its parent."
                );
                return;
            }

            let Some(target) = self.target_node.as_ref() else {
                let path = self.base().get_path();
                godot_error!(
                    "PositionImporter at '{path}' has no 'target_node' set. \
                     Please assign a Node3D in the inspector."
                );
                return;
            };

            let xform = target.get_global_transform();
            let origin = xform.origin;
            let position = Vec3::new(origin.x as f32, origin.y as f32, origin.z as f32);

            godot_print!(
                "[PositionInit] Registering position for godot_id={} from '{}' at ({:.2},{:.2},{:.2})",
                godot_id,
                target.get_name(),
                position.x,
                position.y,
                position.z
            );

            enqueue_position(godot_id, position);
        }

        /// Optional manual trigger.
        #[func]
        fn register(&self) {
            self.register_internal();
        }
    }

    #[godot_api]
    impl INode for PositionImporter {
        fn ready(&mut self) {
            self.register_internal();
        }
    }
}

pub mod sets {
    use bevy::prelude::SystemSet;

    /// Applies position requests after entities exist, but before other post-init importers if desired.
    #[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
    pub struct PositionInitSet;
}

pub mod plugins {
    use super::intentions::InitializePositionIntention;
    use super::sets::PositionInitSet;
    use super::systems::{drain_intentions, handle_initialize_position, log_intentions};
    use bevy::prelude::*;
    use bevy_godot4::import::sets::EntityInitSet;
    use bevy_godot4::prelude::PostEntityInitSet;

    pub struct PositionInitializationPlugin;

    impl Plugin for PositionInitializationPlugin {
        fn build(&self, app: &mut App) {
            app.add_message::<InitializePositionIntention>()
                .configure_sets(
                    FixedUpdate,
                    (PositionInitSet
                        .after(EntityInitSet)
                        .before(PostEntityInitSet),),
                )
                .add_systems(FixedPreUpdate, drain_intentions)
                .add_systems(FixedPreUpdate, log_intentions)
                .add_systems(
                    FixedUpdate,
                    handle_initialize_position.in_set(PositionInitSet),
                );
        }
    }
}
