use crate::export_delivery::ExportChangeSource;
use godot::global::godot_error;
use godot::prelude::*;

/// Basic bridge information for one Godot entity scene.
///
/// Exporters set the entity ID and revision. Nodes made by `with_state_node!`
/// use this information to find the data for the entity.
#[derive(GodotClass)]
#[class(base=Node)]
pub struct EntityMeta {
    subscription_list: Vec<(i64, Callable, Vec<Gd<ExportChangeSource>>)>,
    next_subscription_id: i64,
    despawning: bool,
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
    /// Register only the feature changes this visual needs. Empty sources mean all changes.
    #[func]
    fn watch_changes(&mut self, callback: Callable, sources: Array<Gd<ExportChangeSource>>) -> i64 {
        let id = self.next_subscription_id;
        self.next_subscription_id += 1;
        self.subscription_list
            .push((id, callback, sources.iter_shared().collect()));
        id
    }

    #[func]
    fn unwatch_changes(&mut self, id: i64) {
        self.subscription_list.retain(|entry| entry.0 != id);
    }

    /// Number of active visual listeners, useful when checking scene replacement.
    #[func]
    fn get_change_listener_count(&self) -> i64 {
        self.subscription_list.len() as i64
    }

    pub(crate) fn has_subscription(&self, id: i64) -> bool {
        self.subscription_list.iter().any(|entry| entry.0 == id)
    }

    /// Cancel queued updates before a scene starts its own destruction animation.
    pub fn cancel_updates(&mut self) {
        self.despawning = true;
        self.subscription_list.clear();
    }

    pub(crate) fn accepts_updates(&self) -> bool {
        !self.despawning
    }

    /// Internal delivery entry; release all Rust borrows before calling visual code.
    #[func(gd_self)]
    pub fn _deliver_changes(mut this: Gd<Self>, revision: i64) {
        if !this.bind().accepts_updates() {
            return;
        }
        let callback_list = this.bind_mut().callbacks_for(revision);
        let argument_list = Array::from(&[revision.to_variant()]);
        for (id, callback) in callback_list {
            if !this.is_instance_valid() {
                return;
            }
            if this.bind().has_subscription(id) && callback.is_valid() {
                callback.callv(&argument_list);
            }
        }
        if this.is_instance_valid() && this.bind().accepts_updates() {
            this.signals().on_change().emit(revision);
        }
    }

    pub(crate) fn callbacks_for(&mut self, revision: i64) -> Vec<(i64, Callable)> {
        self.revision = revision;
        self.subscription_list
            .iter()
            .filter(|entry| {
                entry.2.is_empty()
                    || entry
                        .2
                        .iter()
                        .any(|source| source.bind().changed(self.entity_id, revision))
            })
            .map(|entry| (entry.0, entry.1.clone()))
            .collect()
    }
    #[signal]
    pub fn on_change(revision: i64);

    #[signal]
    pub fn on_despawning(entity_id: i64);

    /// Lets this scene handle its own cleanup when Bevy removes the entity.
    #[func]
    pub fn set_custom_cleanup(&mut self, enabled: bool) {
        self.custom_cleanup_enabled = enabled;
    }

    /// Returns whether this scene handles its own cleanup.
    pub fn is_custom_cleanup_enabled(&self) -> bool {
        self.custom_cleanup_enabled
    }

    /// Assigns the stable ID shared with the Bevy entity.
    #[func]
    pub fn assign_entity_id(&mut self, entity_id: i64) {
        self.entity_id = entity_id;
    }

    /// Finds `EntityMeta` at an entity scene root or direct child.
    ///
    /// It uses the root node first, then the first direct child. It prints an
    /// error when the scene is missing this node or has more than one.
    pub fn resolve_from_scene_root(instance: Gd<Node>, entity_id: i64) -> Option<Gd<EntityMeta>> {
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
            godot_error!(
                "Multiple EntityMeta nodes found for entity {}; keep exactly one at scene root or direct child",
                entity_id
            );
        }

        if direct_children_meta.len() > 1 {
            godot_error!(
                "Multiple direct-child EntityMeta nodes found for entity {}; keep exactly one",
                entity_id
            );
        }

        if let Some(meta) = root_meta {
            return Some(meta);
        }

        if let Some(meta) = direct_children_meta.into_iter().next() {
            return Some(meta);
        }

        godot_error!(
            "Spawned scene is missing EntityMeta for entity {}; attach it to root or root direct child",
            entity_id
        );

        None
    }
}

#[godot_api]
impl INode for EntityMeta {
    fn init(base: Base<Node>) -> Self {
        Self {
            entity_id: -1,
            subscription_list: Vec::new(),
            next_subscription_id: 1,
            despawning: false,
            revision: -1,
            custom_cleanup_enabled: false,
            base,
        }
    }
}
