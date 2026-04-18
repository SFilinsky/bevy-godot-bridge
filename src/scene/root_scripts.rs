use crate::scene::scene_root::SceneRoot;
use crate::tools::collect_children;
use godot::global::godot_error;
use godot::prelude::*;

#[derive(Debug)]
pub enum RootScriptsLookupError {
    MissingInParentChain,
    MissingSceneRoot,
    MissingUnderSceneRoot,
    MultipleUnderSceneRoot,
}

impl std::fmt::Display for RootScriptsLookupError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RootScriptsLookupError::MissingInParentChain => {
                write!(f, "No RootScripts found in host parent chain")
            }
            RootScriptsLookupError::MissingSceneRoot => {
                write!(f, "No SceneRoot found in host parent chain")
            }
            RootScriptsLookupError::MissingUnderSceneRoot => {
                write!(
                    f,
                    "No RootScripts found as direct child of resolved SceneRoot"
                )
            }
            RootScriptsLookupError::MultipleUnderSceneRoot => {
                write!(
                    f,
                    "Multiple RootScripts nodes found under resolved SceneRoot"
                )
            }
        }
    }
}

#[derive(GodotClass)]
#[class(init, base = Node)]
pub struct RootScripts {
    #[base]
    base: Base<Node>,
}

impl RootScripts {
    fn resolve_as_parent(start: &Gd<Node>) -> Result<Gd<RootScripts>, RootScriptsLookupError> {
        let mut current: Option<Gd<Node>> = Some(start.clone());

        while let Some(node) = current {
            if let Ok(root_scripts) = node.clone().try_cast::<RootScripts>() {
                return Ok(root_scripts);
            }
            current = node.get_parent();
        }

        Err(RootScriptsLookupError::MissingInParentChain)
    }

    fn resolve_via_scene_root(host: &Gd<Node>) -> Result<Gd<RootScripts>, RootScriptsLookupError> {
        let Some(scene_root) = SceneRoot::resolve_as_parent(host) else {
            return Err(RootScriptsLookupError::MissingSceneRoot);
        };

        let mut nodes = collect_children::<RootScripts>(scene_root.upcast::<Node>(), false);

        if nodes.is_empty() {
            return Err(RootScriptsLookupError::MissingUnderSceneRoot);
        }

        if nodes.len() > 1 {
            return Err(RootScriptsLookupError::MultipleUnderSceneRoot);
        }

        Ok(nodes.remove(0))
    }

    pub fn resolve<T>(host: &Gd<T>) -> Result<Gd<RootScripts>, RootScriptsLookupError>
    where
        T: GodotClass + Inherits<Node>,
    {
        let host_node: Gd<Node> = host.clone().upcast();

        let result = match Self::resolve_as_parent(&host_node) {
            Ok(root_scripts) => Ok(root_scripts),
            Err(_) => Self::resolve_via_scene_root(&host_node),
        };

        if let Err(err) = &result {
            let host_path = if host_node.is_inside_tree() {
                host_node.get_path().to_string()
            } else {
                "<detached host>".to_string()
            };

            godot_error!(
                "RootScripts::resolve() failed for host '{}': {}",
                host_path,
                err
            );
        }

        result
    }
}

#[godot_api]
impl RootScripts {
    #[func]
    pub fn resolve_or_null(host: Gd<Node>) -> Option<Gd<RootScripts>> {
        Self::resolve(&host).ok()
    }
}
