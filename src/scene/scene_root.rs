//! Typed marker for the root of one authored bridge scene.

use godot::prelude::*;
observe! { use crate::performance::{requested, startup::{begin_level, tree_ready, end_level}}; }

/// Marks the nearest scene root used to resolve scene-local bridge nodes.
#[derive(GodotClass)]
#[class(init, base=Node)]
pub struct SceneRoot {
    #[base]
    base: Base<Node>,
}

impl SceneRoot {
    /// Walks parent nodes to find the nearest authored `SceneRoot`.
    pub fn resolve_as_parent(host: &Gd<Node>) -> Option<Gd<SceneRoot>> {
        let mut current: Option<Gd<Node>> = Some(host.clone());

        while let Some(node) = current {
            if let Ok(scene_root) = node.clone().try_cast::<SceneRoot>() {
                return Some(scene_root);
            }
            current = node.get_parent();
        }

        None
    }

    /// Finds nodes below this scene without entering a nested `SceneRoot`.
    ///
    /// A nested scene owns its own bridge nodes. Call this when a lookup must
    /// stay inside the nearest authored scene.
    pub fn collect_descendants<T>(root: Gd<SceneRoot>) -> Vec<Gd<T>>
    where
        T: GodotClass + Inherits<Node>,
    {
        fn visit<T>(node: Gd<Node>, result: &mut Vec<Gd<T>>)
        where
            T: GodotClass + Inherits<Node>,
        {
            for child_index in 0..node.get_child_count() {
                let Some(child) = node.get_child(child_index) else {
                    continue;
                };
                if child.clone().try_cast::<SceneRoot>().is_ok() {
                    continue;
                }
                if let Ok(typed_child) = child.clone().try_cast::<T>() {
                    result.push(typed_child);
                }
                visit(child, result);
            }
        }

        let mut result = Vec::new();
        visit(root.upcast::<Node>(), &mut result);
        result
    }
}

#[godot_api]
impl INode for SceneRoot {
    fn enter_tree(&mut self) {
        observe! {
            if requested() {
                begin_level(self.to_gd().instance_id().to_i64());
            }
        }
    }

    fn ready(&mut self) {
        observe! { tree_ready(self.to_gd().instance_id().to_i64()); }
    }

    fn exit_tree(&mut self) {
        observe! { end_level(self.to_gd().instance_id().to_i64()); }
    }
}

#[godot_api]
impl SceneRoot {
    #[func]
    pub fn resolve_as_parent_or_null(host: Gd<Node>) -> Option<Gd<SceneRoot>> {
        Self::resolve_as_parent(&host)
    }
}
