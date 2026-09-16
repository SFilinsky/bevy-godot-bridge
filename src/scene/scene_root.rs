//! Typed marker for the root of one authored bridge scene.

use godot::prelude::*;

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
}

#[godot_api]
impl SceneRoot {
    #[func]
    pub fn resolve_as_parent_or_null(host: Gd<Node>) -> Option<Gd<SceneRoot>> {
        Self::resolve_as_parent(&host)
    }
}
