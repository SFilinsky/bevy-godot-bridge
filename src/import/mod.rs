/// Bevy-side component that tags entities originating from Godot and lets
/// other importer systems (e.g., colliders) find the right Bevy entity by ID.
pub mod components {
    use bevy::prelude::Component;

    #[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct GodotEntity {
        pub godot_id: i64,
    }
}

pub mod intentions {
    use bevy::prelude::{Message, Vec3};

    /// Intention coming from Godot to initialize a backend entity.
    /// Carries a Godot-generated stable ID to allow follow-up requests
    /// (collider, archetype data, etc.) to target the correct Bevy entity.
    #[derive(Message, Debug, Clone)]
    pub struct InitializeEntityIntention {
        pub godot_id: i64,
        pub name: String,
        pub position: Vec3,
    }
}

mod systems {
    use super::components::GodotEntity;
    use super::intentions::InitializeEntityIntention;
    use bevy::prelude::*;
    use godot::global::godot_print;
    use once_cell::sync::Lazy;
    use std::collections::VecDeque;
    use std::sync::Mutex;

    // Local queue (per Bevy app), filled from Godot via the importer node.
    static INIT_QUEUE: Lazy<Mutex<VecDeque<InitializeEntityIntention>>> =
        Lazy::new(|| Mutex::new(VecDeque::new()));

    /// Called from the Godot-facing importer (in the same process).
    pub fn enqueue_entity(godot_id: i64, name: String, position: Vec3) {
        INIT_QUEUE
            .lock()
            .unwrap()
            .push_back(InitializeEntityIntention {
                godot_id,
                name,
                position,
            });
    }

    /// Drain local queue into Bevy's message channel.
    pub(super) fn drain_intentions(mut out: MessageWriter<InitializeEntityIntention>) {
        let mut q = INIT_QUEUE.lock().unwrap();
        for i in q.drain(..) {
            out.write(i);
        }
    }

    /// Optional logging to confirm we actually got messages.
    pub(super) fn log_intentions(mut ev: MessageReader<InitializeEntityIntention>) {
        for InitializeEntityIntention {
            godot_id,
            name,
            position,
        } in ev.read().cloned()
        {
            godot_print!(
                "[EntityInit] request godot_id={} name='{}' at ({:.2},{:.2},{:.2})",
                godot_id,
                name,
                position.x,
                position.y,
                position.z
            );
        }
    }

    /// Handle entity initialization on the Bevy side.
    ///
    /// For now this:
    /// - spawns a fixed rigid body at the given position
    /// - tags entity with `GodotEntity { godot_id }`
    pub(super) fn handle_initialize_entity(
        mut ev: MessageReader<InitializeEntityIntention>,
        mut cmd: Commands,
    ) {
        for InitializeEntityIntention {
            godot_id,
            name,
            position,
        } in ev.read().cloned()
        {
            cmd.spawn((
                Name::new(format!("GodotLevelEntity:{name}")),
                GodotEntity { godot_id },
                Transform::from_translation(position),
            ));
        }
    }
}

pub mod importers {
    use super::systems::enqueue_entity;
    use bevy::prelude::Vec3;
    use godot::classes::{Node, Node3D};
    use godot::global::{godot_error, godot_print};
    use godot::obj::Base;
    use godot::prelude::*;
    use std::sync::atomic::{AtomicU64, Ordering};

    /// Global ID generator for Godot-authored entities.
    ///
    /// - IDs are generated on the Godot side *before* any Bevy systems run,
    ///   so collider/other requests can reference them immediately.
    /// - IDs are unique within the process lifetime.
    static NEXT_ENTITY_ID: AtomicU64 = AtomicU64::new(1);

    fn alloc_entity_id() -> u64 {
        // Relaxed is fine: uniqueness is what matters.
        NEXT_ENTITY_ID.fetch_add(1, Ordering::Relaxed)
    }

    /// Godot-facing node that registers an entity into Bevy.
    ///
    /// Key additions:
    /// - auto-generated unique `entity_id` (Godot-side)
    /// - exposed getter so other scripts can read it and send follow-up requests
    #[derive(GodotClass)]
    #[class(init, base=Node)]
    pub struct EntityImporter {
        /// Root node of the entity in the Godot scene.
        ///
        /// REQUIRED: if not set, we log an error and skip registration.
        #[export]
        #[var]
        entity_root: Option<Gd<Node3D>>,

        /// Godot-generated unique ID for this entity.
        ///
        /// We generate it automatically if it's 0.
        /// Exposed so other importer nodes/scripts can reference it.
        #[export]
        #[var]
        entity_id: i64,

        #[base]
        base: Base<Node>,
    }

    #[godot_api]
    impl EntityImporter {
        /// Ensures the entity has a valid ID and returns it as u64.
        fn generate_entity_id(&mut self) -> u64 {
            if self.entity_id <= 0 {
                let entity_id = alloc_entity_id();
                // Store as i64 for Godot friendliness.
                self.entity_id = entity_id as i64;
            }
            self.entity_id as u64
        }

        /// Manually trigger registration (optional).
        /// Normally `_ready()` triggers it automatically.
        #[func]
        fn register(&mut self) {
            self.register_internal();
        }

        fn register_internal(&mut self) {
            let Some(root) = self.entity_root.as_ref() else {
                let path = self.base().get_path();
                godot_error!(
                    "EntityImporter at '{path}' has no 'entity_root' set. \
                     Please assign a Node3D in the inspector."
                );
                return;
            };

            let godot_id = self.entity_id;

            // Name of the entity comes from the entity_root node.
            let name = root.get_name().to_string();

            // World position of the entity root.
            let xform = root.get_global_transform();
            let origin = xform.origin;
            let position = Vec3::new(origin.x as f32, origin.y as f32, origin.z as f32);

            godot_print!(
                "[EntityInit] Registering godot_id={} name='{}' at ({:.2},{:.2},{:.2})",
                godot_id,
                name,
                position.x,
                position.y,
                position.z
            );

            enqueue_entity(godot_id, name, position);
        }
    }

    #[godot_api]
    impl INode for EntityImporter {
        fn enter_tree(&mut self) {
            self.generate_entity_id();
        }

        fn ready(&mut self) {
            // ID must exist as early as possible so sibling importer nodes
            // can query it during the same frame if they need to.
            self.register_internal();
        }
    }
}

pub mod sets {
    use bevy::prelude::SystemSet;

    #[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
    pub struct EntityInitSet;

    #[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
    pub struct PostEntityInitSet;
}

pub mod plugins {
    use super::intentions::InitializeEntityIntention;
    use super::systems::{drain_intentions, handle_initialize_entity, log_intentions};
    use crate::import::sets::{EntityInitSet, PostEntityInitSet};
    use bevy::prelude::*;

    pub struct EntityInitializationPlugin;

    impl Plugin for EntityInitializationPlugin {
        fn build(&self, app: &mut App) {
            app.add_message::<InitializeEntityIntention>()
                .configure_sets(FixedUpdate, (EntityInitSet.before(PostEntityInitSet),))
                // Ensure we capture all Godot enqueued requests before FixedUpdate.
                .add_systems(FixedPreUpdate, drain_intentions)
                // Optional debug logging.
                .add_systems(FixedPreUpdate, log_intentions)
                // Spawn entities
                .add_systems(FixedUpdate, handle_initialize_entity.in_set(EntityInitSet));
        }
    }
}
