use crate::tools::collect_children;
use godot::prelude::*;

#[derive(GodotClass)]
#[class(base=Node)]
pub struct EntityMeta {
    #[var]
    pub entity_id: i64,
    #[var]
    pub revision: i64,
    #[var]
    custom_cleanup_enabled: bool,
    #[base]
    base: Base<Node>,
}

#[godot_api]
impl EntityMeta {
    #[signal]
    pub fn on_change(revision: i64);

    #[signal]
    pub fn on_despawning(entity_id: i64);

    #[func]
    pub fn set_custom_cleanup(&mut self, enabled: bool) {
        self.custom_cleanup_enabled = enabled;
    }

    pub fn is_custom_cleanup_enabled(&self) -> bool {
        self.custom_cleanup_enabled
    }

    #[func]
    pub fn assign_entity_id(&mut self, entity_id: i64) {
        self.entity_id = entity_id;
    }

    pub fn resolve_from_scene_root(instance: Gd<Node>, entity_id: i64) -> Gd<EntityMeta> {
        let root_meta = instance.clone().try_cast::<EntityMeta>().ok();

        let mut direct_children_meta: Vec<Gd<EntityMeta>> = Vec::new();
        let child_count = instance.get_child_count();
        for idx in 0..child_count {
            let Some(child) = instance.get_child(idx) else {
                continue;
            };

            if let Ok(meta) = child.try_cast::<EntityMeta>() {
                direct_children_meta.push(meta);
            }
        }

        if root_meta.is_some() && !direct_children_meta.is_empty() {
            godot_warn!(
                "Multiple EntityMeta nodes found for entity {}; using root-attached one",
                entity_id
            );
        }

        if direct_children_meta.len() > 1 {
            godot_warn!(
                "Multiple direct-child EntityMeta nodes found for entity {}; using the first one",
                entity_id
            );
        }

        if let Some(meta) = root_meta {
            return meta;
        }

        if let Some(meta) = direct_children_meta.into_iter().next() {
            return meta;
        }

        let nested_meta = collect_children::<EntityMeta>(instance.clone(), true);
        if !nested_meta.is_empty() {
            panic!(
                "EntityMeta must be attached to scene root or as a direct child of the root for entity {}; deeper nested placement is invalid",
                entity_id
            );
        }

        panic!(
            "Spawned scene is missing EntityMeta for entity {}; attach it to root or root direct child",
            entity_id
        );
    }
}

#[godot_api]
impl INode for EntityMeta {
    fn init(base: Base<Node>) -> Self {
        Self {
            entity_id: -1,
            revision: -1,
            custom_cleanup_enabled: false,
            base,
        }
    }
}
