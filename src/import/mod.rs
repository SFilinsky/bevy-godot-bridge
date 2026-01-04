pub mod position;
pub mod components {
    use bevy::prelude::Component;

    #[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct GodotEntity {
        pub godot_id: i64,
    }
}

pub mod intentions {
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

        fn update_dto(dto: &mut Gd<Self::DtoType>, data: &Self::DataType) {
            let mut d = dto.bind_mut();
            d.godot_id = data.godot_id;
            d.name = GString::from(data.name.as_str());
        }

        fn update_data(dto: &Gd<Self::DtoType>, data: &mut Self::DataType) {
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
    use super::components::GodotEntity;
    use super::intentions::InitializeEntityIntention;
    use bevy::prelude::*;

    pub(super) fn handle_initialize_entity(
        mut ev: MessageReader<InitializeEntityIntention>,
        mut cmd: Commands,
    ) {
        for InitializeEntityIntention { godot_id, name } in ev.read().cloned() {
            cmd.spawn((
                Name::new(format!("GodotLevelEntity:{name}")),
                GodotEntity { godot_id },
            ));
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
    use bevy_godot4::BevyApp;
    use godot::classes::Node;
    use godot::global::godot_print;
    use godot::obj::Base;
    use godot::prelude::*;
    use std::sync::atomic::{AtomicU64, Ordering};

    static NEXT_ENTITY_ID: AtomicU64 = AtomicU64::new(1);
    fn alloc_entity_id() -> u64 {
        NEXT_ENTITY_ID.fetch_add(1, Ordering::Relaxed)
    }

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
        fn generate_entity_id(&mut self) {
            if self.entity_id <= 0 {
                self.entity_id = alloc_entity_id() as i64;
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
                let node = &self.base().clone().upcast();
                let mut queue = self.queue.bind_mut();
                queue.bind_bevy_app(BevyApp::find_for(node).unwrap());
            }

            self.generate_entity_id();
        }

        fn ready(&mut self) {
            self.enqueue_init();
        }
    }
}

pub mod plugins {
    use super::sets::{EntityInitSet, PostEntityInitSet};
    use super::systems::handle_initialize_entity;
    use bevy::prelude::*;

    pub struct EntityInitializationPlugin;

    impl Plugin for EntityInitializationPlugin {
        fn build(&self, app: &mut App) {
            app.add_plugins(super::intentions::InitializeEntityIntentionImportPlugin)
                .configure_sets(FixedUpdate, (EntityInitSet.before(PostEntityInitSet),))
                .add_systems(FixedUpdate, handle_initialize_entity.in_set(EntityInitSet));
        }
    }
}
