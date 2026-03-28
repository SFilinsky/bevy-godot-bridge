use crate::entity_registry::EntityRegistry;
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
    pub fn on_removed(entity_id: i64);

    #[func]
    pub fn set_custom_cleanup(&mut self, enabled: bool) {
        self.custom_cleanup_enabled = enabled;
    }

    pub fn is_custom_cleanup_enabled(&self) -> bool {
        self.custom_cleanup_enabled
    }

    #[func]
    pub fn assign_entity_id(&mut self, entity_id: i64) {
        if self.entity_id == entity_id {
            self.register_in_registry();
            return;
        }

        if self.entity_id >= 0 {
            self.unregister_from_registry();
        }

        self.entity_id = entity_id;
        self.register_in_registry();
    }

    fn find_registry(&self) -> Option<Gd<EntityRegistry>> {
        let tree = self.base().get_tree()?;
        let root = tree.get_root()?;
        let app = root.try_get_node_as::<Node>("BevyAppSingleton")?;
        app.try_get_node_as::<EntityRegistry>("EntityRegistry")
    }

    fn register_in_registry(&mut self) {
        if self.entity_id < 0 {
            return;
        }

        let Some(mut registry) = self.find_registry() else {
            return;
        };

        let node = self.base().clone().upcast::<Node>();
        let Ok(meta) = node.try_cast::<EntityMeta>() else {
            return;
        };

        registry
            .bind_mut()
            .register_entity_meta(self.entity_id, meta);
    }

    fn unregister_from_registry(&mut self) {
        if self.entity_id < 0 {
            return;
        }

        let Some(mut registry) = self.find_registry() else {
            return;
        };

        registry.bind_mut().unregister_entity_meta(self.entity_id);
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

    fn ready(&mut self) {
        self.register_in_registry();
    }

    fn exit_tree(&mut self) {
        self.unregister_from_registry();
    }
}
