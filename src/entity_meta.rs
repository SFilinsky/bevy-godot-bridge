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

    #[func]
    pub fn set_custom_cleanup(&mut self, enabled: bool) {
        self.custom_cleanup_enabled = enabled;
    }

    pub fn is_custom_cleanup_enabled(&self) -> bool {
        self.custom_cleanup_enabled
    }

    #[func(virtual)]
    pub fn on_removed(&mut self) {}
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
