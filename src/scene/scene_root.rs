use godot::prelude::*;

#[derive(GodotClass)]
#[class(init, base=Node)]
pub struct SceneRoot {
    #[base]
    base: Base<Node>,
}

impl SceneRoot {
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
