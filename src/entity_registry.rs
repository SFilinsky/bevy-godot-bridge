use crate::entity_meta::EntityMeta;
use godot::prelude::*;
use std::collections::HashMap;

#[derive(GodotClass)]
#[class(base=Node)]
pub struct EntityRegistry {
    metas: HashMap<i64, Gd<EntityMeta>>,
    #[base]
    base: Base<Node>,
}

#[godot_api]
impl EntityRegistry {
    #[func]
    pub fn register_entity_meta(&mut self, entity_id: i64, meta: Gd<EntityMeta>) {
        if entity_id < 0 || !meta.is_instance_valid() {
            return;
        }

        self.metas.insert(entity_id, meta);
    }

    #[func]
    pub fn unregister_entity_meta(&mut self, entity_id: i64) {
        self.metas.remove(&entity_id);
    }

    #[func]
    pub fn get_entity_meta_or_null(&mut self, entity_id: i64) -> Option<Gd<EntityMeta>> {
        let Some(meta) = self.metas.get(&entity_id).cloned() else {
            return None;
        };

        if meta.is_instance_valid() {
            Some(meta)
        } else {
            self.metas.remove(&entity_id);
            None
        }
    }

    #[func]
    pub fn get_entity_meta_list(&mut self) -> Array<Gd<EntityMeta>> {
        let mut out: Array<Gd<EntityMeta>> = Array::new();
        let mut stale_ids: Vec<i64> = Vec::new();

        for (entity_id, meta) in self.metas.iter() {
            if meta.is_instance_valid() {
                out.push(meta);
            } else {
                stale_ids.push(*entity_id);
            }
        }

        for entity_id in stale_ids {
            self.metas.remove(&entity_id);
        }

        out
    }

    #[func]
    pub fn resolve_entity_meta_from_node(&mut self, mut node: Gd<Node>) -> Option<Gd<EntityMeta>> {
        if !node.is_instance_valid() {
            return None;
        }

        loop {
            if let Ok(meta) = node.clone().try_cast::<EntityMeta>() {
                if meta.is_instance_valid() {
                    let entity_id = meta.bind().entity_id;
                    if entity_id >= 0 {
                        self.metas.insert(entity_id, meta.clone());
                    }
                    return Some(meta);
                }
                return None;
            }

            let Some(parent) = node.get_parent() else {
                return None;
            };

            node = parent;
        }
    }
}

#[godot_api]
impl INode for EntityRegistry {
    fn init(base: Base<Node>) -> Self {
        Self {
            metas: HashMap::new(),
            base,
        }
    }
}
