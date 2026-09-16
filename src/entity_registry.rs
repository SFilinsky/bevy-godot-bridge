use crate::entity_meta::EntityMeta;
use crate::prelude::BevyApp;
use godot::prelude::*;
use std::collections::HashMap;

/// Finds Godot entity information from a bridge entity ID.
///
/// When Bevy creates an entity scene, it registers its `EntityMeta` here. Godot
/// can then find that node without searching the whole scene tree.
#[derive(GodotClass)]
#[class(base=Node)]
pub struct EntityRegistry {
    metas: HashMap<i64, Gd<EntityMeta>>,
    #[base]
    base: Base<Node>,
}

#[godot_api]
impl EntityRegistry {
    /// Finds the registry for the nearest `BevyApp`.
    // NOTE:
    // Registry entries are currently managed by Bevy-driven spawners only.
    // Scene-authored EntityMeta nodes that are not spawned by Bevy are not
    // auto-registered in this phase.
    pub fn resolve(host: &Gd<Node>) -> Option<Gd<EntityRegistry>> {
        let Ok(app) = BevyApp::resolve(host) else {
            return None;
        };

        app.try_get_node_as::<EntityRegistry>("EntityRegistry")
    }

    /// Finds the nearest registry or returns nothing when the scene has none.
    #[func]
    pub fn resolve_or_null(host: Gd<Node>) -> Option<Gd<EntityRegistry>> {
        Self::resolve(&host)
    }

    /// Stores the Godot metadata node for one bridge entity ID.
    #[func]
    pub fn register_entity_meta(&mut self, entity_id: i64, meta: Gd<EntityMeta>) {
        if entity_id < 0 || !meta.is_instance_valid() {
            return;
        }

        self.metas.insert(entity_id, meta);
    }

    /// Removes the metadata node for one bridge entity ID.
    #[func]
    pub fn unregister_entity_meta(&mut self, entity_id: i64) {
        self.metas.remove(&entity_id);
    }

    /// Returns metadata for one ID and removes it if Godot already freed it.
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

    /// Returns all valid metadata nodes and removes entries Godot has freed.
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
