pub mod position;
pub mod components {
    use bevy::prelude::Component;

    #[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub(crate) struct GodotIdentity {
        pub godot_id: i64,
    }
}

pub mod subsystems {
    use super::components::GodotIdentity;
    use crate::app::BevyAppSubsystem;
    use bevy::ecs::entity::Entities;
    use bevy::ecs::system::SystemParam;
    use bevy::prelude::{Commands, Entity, Query, ResMut, Resource};
    use std::collections::HashMap;

    #[derive(Resource, Default, Debug)]
    pub(crate) struct IdentityRegistry {
        by_entity: HashMap<Entity, i64>,
        by_id: HashMap<i64, Entity>,
    }

    #[derive(SystemParam)]
    pub struct IdentitySubsystem<'w, 's> {
        identities: Query<'w, 's, (Entity, &'static GodotIdentity)>,
        registry: ResMut<'w, IdentityRegistry>,
        entities: &'w Entities,
        app: BevyAppSubsystem<'w, 's>,
        commands: Commands<'w, 's>,
    }

    impl<'w, 's> IdentitySubsystem<'w, 's> {
        pub fn bind_identity(&mut self, entity: Entity, godot_id: i64) {
            if let Some(old_id) = self.registry.by_entity.insert(entity, godot_id) {
                self.registry.by_id.remove(&old_id);
            }

            if let Some(previous_entity) = self.registry.by_id.insert(godot_id, entity) {
                self.registry.by_entity.remove(&previous_entity);

                if previous_entity != entity {
                    self.commands
                        .entity(previous_entity)
                        .remove::<GodotIdentity>();
                }
            }

            self.commands
                .entity(entity)
                .insert(GodotIdentity { godot_id });
        }

        pub fn get_identity(&mut self, entity: Entity) -> i64 {
            if let Some(id) = self.registry.by_entity.get(&entity).copied() {
                return id;
            }

            if let Ok((_, identity)) = self.identities.get(entity) {
                let id = identity.godot_id;
                self.registry.by_entity.insert(entity, id);
                self.registry.by_id.insert(id, entity);
                return id;
            }

            let id = self.app.alloc_entity_id();
            self.bind_identity(entity, id);
            id
        }

        pub fn try_get_identity(&mut self, entity: Entity) -> Option<i64> {
            if let Some(id) = self.registry.by_entity.get(&entity).copied() {
                return Some(id);
            }

            if let Ok((_, identity)) = self.identities.get(entity) {
                let id = identity.godot_id;
                self.registry.by_entity.insert(entity, id);
                self.registry.by_id.insert(id, entity);
                return Some(id);
            }

            None
        }

        pub fn resolve_entity(&mut self, godot_id: i64) -> Option<Entity> {
            if let Some(entity) = self.registry.by_id.get(&godot_id).copied() {
                if self.entities.contains(entity) {
                    return Some(entity);
                }

                self.registry.by_id.remove(&godot_id);
                self.registry.by_entity.remove(&entity);
            }

            if let Some(entity) = self
                .identities
                .iter()
                .find_map(|(entity, identity)| (identity.godot_id == godot_id).then_some(entity))
            {
                self.bind_identity(entity, godot_id);
                return Some(entity);
            }

            None
        }
    }
}

pub mod intentions {
    use crate::import::subsystems::IdentitySubsystem;
    use bevy::prelude::Message;
    use bevy_godot4::dto::DataTransferConfig;
    use bevy_godot4_proc_macros::import_queue;
    use godot::obj::Base;
    use godot::prelude::*;

    #[derive(Message, Debug, Clone, Default)]
    pub struct InitializeEntityIntention {
        pub godot_id: i64,
        pub name: String,
    }

    /// Godot DTO for InitializeEntityIntention (Godot-friendly types)
    #[derive(GodotClass, Debug)]
    #[class(init, base=RefCounted)]
    pub struct InitializeEntityIntentionDto {
        #[var]
        pub godot_id: i64,

        #[var]
        pub name: GString,

        #[base]
        base: Base<RefCounted>,
    }

    pub struct InitializeEntityIntentionTransferConfig;
    impl DataTransferConfig for InitializeEntityIntentionTransferConfig {
        type DataType = InitializeEntityIntention;
        type DtoType = InitializeEntityIntentionDto;

        fn update_dto(
            dto: &mut Gd<Self::DtoType>,
            data: &Self::DataType,
            _identity: &mut IdentitySubsystem,
        ) {
            let mut d = dto.bind_mut();
            d.godot_id = data.godot_id;
            d.name = GString::from(data.name.as_str());
        }

        fn update_data(
            dto: &Gd<Self::DtoType>,
            data: &mut Self::DataType,
            _identity: &mut IdentitySubsystem,
        ) {
            let d = dto.bind();
            data.godot_id = d.godot_id;
            data.name = d.name.to_string();
        }
    }

    import_queue! {
        config: InitializeEntityIntentionTransferConfig,
    }
}

mod systems {
    use super::intentions::InitializeEntityIntention;
    use super::subsystems::IdentitySubsystem;
    use bevy::prelude::*;

    pub(super) fn handle_initialize_entity(
        mut ev: MessageReader<InitializeEntityIntention>,
        mut cmd: Commands,
        mut identity: IdentitySubsystem,
    ) {
        for InitializeEntityIntention { godot_id, name } in ev.read().cloned() {
            let entity = cmd
                .spawn(Name::new(format!("GodotLevelEntity:{name}")))
                .id();
            identity.bind_identity(entity, godot_id);
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

pub mod importers {
    use crate::import::intentions::{InitializeEntityIntentionDto, InitializeEntityIntentionQueue};
    use bevy_godot4::prelude::BevyApp;
    use godot::classes::Node;
    use godot::global::godot_print;
    use godot::obj::Base;
    use godot::prelude::*;

    #[derive(GodotClass)]
    #[class(base=Node)]
    pub struct EntityImporter {
        #[var]
        entity_id: i64,

        #[var]
        entity_name: GString,

        queue: Gd<InitializeEntityIntentionQueue>,

        #[base]
        base: Base<Node>,
    }

    #[godot_api]
    impl EntityImporter {
        fn generate_entity_id(&mut self, app: &mut Gd<BevyApp>) {
            if self.entity_id <= 0 {
                self.entity_id = app.bind_mut().alloc_entity_id();
            }
        }

        fn derive_name(&self) -> String {
            let n = self.entity_name.to_string();
            if n.trim().is_empty() {
                self.base().get_name().to_string()
            } else {
                n
            }
        }

        fn enqueue_init(&mut self) {
            let godot_id = self.entity_id;
            let name = self.derive_name();

            let mut dto: Gd<InitializeEntityIntentionDto> = InitializeEntityIntentionDto::new_gd();
            {
                let mut d = dto.bind_mut();
                d.godot_id = godot_id;
                d.name = name.as_str().into();
            }

            godot_print!("[EntityInit] enqueue godot_id={} name='{}'", godot_id, name);

            self.queue.bind_mut().enqueue(dto);
        }
    }

    #[godot_api]
    impl INode for EntityImporter {
        fn init(base: Base<Node>) -> Self {
            Self {
                entity_id: 0,
                entity_name: GString::new(),
                queue: InitializeEntityIntentionQueue::new_gd(),
                base,
            }
        }

        fn enter_tree(&mut self) {
            {
                let node = &self.base().clone().upcast::<Node>();
                let mut app = BevyApp::resolve(node).expect("BevyApp not found");

                {
                    let mut queue = self.queue.bind_mut();
                    queue.bind_bevy_app(app.clone());
                }

                self.generate_entity_id(&mut app);
            }
        }

        fn ready(&mut self) {
            self.enqueue_init();
        }
    }
}

pub mod plugins {
    use super::sets::{EntityInitSet, PostEntityInitSet};
    use super::subsystems::IdentityRegistry;
    use super::systems::handle_initialize_entity;
    use bevy::prelude::*;

    pub(crate) struct IdentitySubsystemPlugin;

    impl Plugin for IdentitySubsystemPlugin {
        fn build(&self, app: &mut App) {
            app.init_resource::<IdentityRegistry>();
        }
    }

    pub struct EntityInitializationPlugin;

    impl Plugin for EntityInitializationPlugin {
        fn build(&self, app: &mut App) {
            app.add_plugins(super::intentions::InitializeEntityIntentionImportPlugin)
                .configure_sets(FixedUpdate, (EntityInitSet.before(PostEntityInitSet),))
                .add_systems(FixedUpdate, handle_initialize_entity.in_set(EntityInitSet));
        }
    }
}
