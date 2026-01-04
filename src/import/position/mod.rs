pub mod intentions {
    use crate::dto::DataTransferConfig;
    use bevy::prelude::{Message, Vec3};
    use bevy_godot4_proc_macros::import_queue;
    use godot::obj::Base;
    use godot::prelude::*;

    /// Request to set/update position of an existing Bevy entity identified by `godot_id`.
    #[derive(Message, Debug, Clone, Copy, Default)]
    pub struct InitializePositionIntention {
        pub godot_id: i64,
        pub position: Vec3,
    }

    /// Godot DTO (Godot-friendly types)
    #[derive(GodotClass, Debug)]
    #[class(init, base=RefCounted)]
    pub struct InitializePositionIntentionDto {
        #[var]
        pub godot_id: i64,

        #[var]
        pub position: Vector3,

        #[base]
        base: Base<RefCounted>,
    }

    pub struct InitializePositionIntentionTransferConfig;
    impl DataTransferConfig for InitializePositionIntentionTransferConfig {
        type DataType = InitializePositionIntention;
        type DtoType = InitializePositionIntentionDto;

        fn update_dto(dto: &mut Gd<Self::DtoType>, data: &Self::DataType) {
            let mut d = dto.bind_mut();
            d.godot_id = data.godot_id;
            d.position = Vector3::new(data.position.x, data.position.y, data.position.z);
        }

        fn update_data(dto: &Gd<Self::DtoType>, data: &mut Self::DataType) {
            let d = dto.bind();
            data.godot_id = d.godot_id;
            data.position = Vec3::new(
                d.position.x as f32,
                d.position.y as f32,
                d.position.z as f32,
            );
        }
    }

    import_queue! {
        config: InitializePositionIntentionTransferConfig,
    }
}

mod systems {
    use super::intentions::InitializePositionIntention;
    use bevy::prelude::*;
    use bevy_godot4::prelude::GodotEntity;
    use godot::global::godot_error;

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
    use crate::BevyApp;
    use crate::import::importers::EntityImporter;
    use crate::import::position::intentions::{
        InitializePositionIntentionDto, InitializePositionIntentionQueue,
    };
    use godot::classes::{Node, Node3D};
    use godot::global::{godot_error, godot_print};
    use godot::obj::Base;
    use godot::prelude::*;

    #[derive(GodotClass)]
    #[class(base=Node)]
    pub struct PositionImporter {
        #[export]
        #[var]
        target_node: Option<Gd<Node3D>>,

        queue: Gd<InitializePositionIntentionQueue>,

        #[base]
        base: Base<Node>,
    }

    #[godot_api]
    impl PositionImporter {
        fn read_parent_entity_id(&self) -> Option<i64> {
            let parent = self.base().get_parent()?;
            let anchor = parent.try_cast::<EntityImporter>().ok()?;
            Some(anchor.bind().get_entity_id())
        }

        fn register_internal(&mut self) {
            let Some(godot_id) = self.read_parent_entity_id() else {
                let path = self.base().get_path();
                godot_error!(
                    "PositionImporter at '{}' must be a child of EntityImporter",
                    path
                );
                return;
            };

            // Clone target out first to avoid borrow conflicts
            let target: Gd<Node3D> = match self.target_node.as_ref().cloned() {
                Some(t) => t,
                None => {
                    let path = self.base().get_path();
                    godot_error!("PositionImporter at '{}' has no 'target_node' set.", path);
                    return;
                }
            };

            let origin = target.get_global_transform().origin;
            let pos = Vector3::new(origin.x, origin.y, origin.z);

            let mut dto: Gd<InitializePositionIntentionDto> =
                InitializePositionIntentionDto::new_gd();
            {
                let mut d = dto.bind_mut();
                d.godot_id = godot_id;
                d.position = pos;
            }

            godot_print!(
                "[PositionInit] enqueue godot_id={} from '{}' at ({:.2},{:.2},{:.2})",
                godot_id,
                target.get_name(),
                pos.x,
                pos.y,
                pos.z
            );

            self.queue.bind_mut().enqueue(dto);
        }
    }

    #[godot_api]
    impl INode for PositionImporter {
        fn init(base: Base<Node>) -> Self {
            Self {
                target_node: None,
                queue: InitializePositionIntentionQueue::new_gd(),
                base,
            }
        }

        fn enter_tree(&mut self) {
            let node = &self.base().clone().upcast();
            let mut queue = self.queue.bind_mut();
            queue.bind_bevy_app(BevyApp::find_for(node).unwrap());
        }

        fn ready(&mut self) {
            self.register_internal();
        }
    }
}

pub mod sets {
    use bevy::prelude::SystemSet;

    #[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
    pub struct PositionInitSet;
}

pub mod plugins {
    use super::sets::PositionInitSet;
    use super::systems::handle_initialize_position;
    use bevy::prelude::*;
    use bevy_godot4::import::sets::EntityInitSet;
    use bevy_godot4::prelude::PostEntityInitSet;

    pub struct PositionInitializationPlugin;

    impl Plugin for PositionInitializationPlugin {
        fn build(&self, app: &mut App) {
            app.add_plugins(super::intentions::InitializePositionIntentionImportPlugin)
                .configure_sets(
                    FixedUpdate,
                    (PositionInitSet
                        .after(EntityInitSet)
                        .before(PostEntityInitSet),),
                )
                .add_systems(
                    FixedUpdate,
                    handle_initialize_position.in_set(PositionInitSet),
                );
        }
    }
}
