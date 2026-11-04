//! Preserve early bridge startup while normal lookups use NodeRegistry.
use super::node_registry::NodeRegistry;
use super::scene_root::SceneRoot;
use godot::builtin::Callable;
use godot::classes::Node;
use godot::global::godot_error;
use godot::obj::{Base, Gd, GodotClass, Inherits};

pub(crate) fn register_level_service_on_entry(base: &Base<Node>) {
    register_on_entry(base, true);
}

/// Constructor hook used by generated stores. Feature code uses NodeRegistry instead.
#[doc(hidden)]
pub fn register_store_on_entry(base: &Base<Node>) {
    register_on_entry(base, false);
}

fn register_on_entry(base: &Base<Node>, level_service: bool) {
    let mut node = base.to_init_gd();
    let id = node.instance_id();
    // Signals survive script overrides without introducing a notification callback
    // that could reborrow BevyApp while a running system adds or removes children.
    let callback = Callable::from_fn("register bridge service on entry", move |_| {
        let Ok(node) = Gd::<Node>::try_from_instance_id(id) else {
            return;
        };
        let result = if level_service {
            if SceneRoot::resolve_as_parent(&node).is_none() {
                return;
            }
            NodeRegistry::register_level_singleton(&node)
        } else if let Some(owner) = node.get_parent() {
            NodeRegistry::register_owned_singleton(&node, &owner)
        } else {
            return;
        };
        if let Err(error) = result {
            godot_error!("Bridge service registration: {error}");
        }
    });
    node.connect("tree_entered", &callback);
}

pub(crate) fn discover<T: GodotClass + Inherits<Node>>(
    branch: &Gd<Node>,
    recursive: bool,
) -> Vec<Gd<T>> {
    branch
        .find_children_ex("*")
        .type_(&T::class_id().to_string())
        .recursive(recursive)
        .owned(false)
        .done()
        .iter_shared()
        .filter_map(|node| node.try_cast().ok())
        .collect()
}

/// Generated stores are direct children of their app. During construction, later siblings
/// may already have a parent but not yet be in Godot's live index.
pub fn child_services<T: GodotClass + Inherits<Node>>(app: Gd<Node>) -> Vec<Gd<T>> {
    if app.is_inside_tree() && app.is_node_ready() && !has_pending_children(&app) {
        return match NodeRegistry::find_owned_singleton::<T>(&app) {
            Ok(node) => node.into_iter().collect(),
            Err(error) => {
                godot_error!("State store lookup: {error}");
                Vec::new()
            }
        };
    }
    discover(&app, false)
}

pub(crate) fn scene_services<T: GodotClass + Inherits<Node>>(
    root: &Gd<SceneRoot>,
    host: &Gd<Node>,
) -> Vec<Gd<T>> {
    let branch = root.clone().upcast::<Node>();
    if !branch.is_inside_tree() {
        return discover::<T>(&branch, true)
            .into_iter()
            .filter(|node| belongs(node, root))
            .collect();
    };
    let mut found: Vec<_> = match NodeRegistry::find_level_singleton::<T>(&branch) {
        Ok(node) => node.into_iter().collect(),
        Err(error) => {
            godot_error!("Scene service lookup: {error}");
            Vec::new()
        }
    };

    // Tree-entry callbacks can ask for a sibling before its own entry. The level captured
    // that authored subtree once. Check only candidates Godot has not indexed yet.
    if let Some(ids) = &root.bind().entering_services {
        for &id in ids {
            if let Ok(node) = Gd::<T>::try_from_instance_id(id) {
                if !node.clone().upcast::<Node>().is_inside_tree() && belongs(&node, root) {
                    found.push(node);
                }
            }
        }
    }
    {
        // Godot keeps is_node_ready() true when a previously ready subtree re-enters.
        // Later siblings are already parented but are not in the live index yet.
        let mut current = Some(host.clone());
        let mut mounting = None;
        while let Some(node) = current {
            // A new consumer has no snapshot entry; adding siblings changes the count.
            // A missing/replaced provider always permits discovery, even if a removal
            // and insertion happened to leave the child count unchanged.
            let unchanged = !found.is_empty()
                && root
                    .bind()
                    .entering_children
                    .as_ref()
                    .and_then(|child_map| child_map.get(&node.instance_id()))
                    .is_some_and(|count| *count == node.get_child_count());
            if !unchanged && has_pending_children(&node) {
                mounting = Some(node.clone());
            }
            if node == branch {
                break;
            }
            current = node.get_parent();
        }
        if let Some(subtree) = mounting {
            for node in discover::<T>(&subtree, true) {
                if !node.clone().upcast::<Node>().is_inside_tree()
                    && belongs(&node, root)
                    && !found
                        .iter()
                        .any(|existing| existing.instance_id() == node.instance_id())
                {
                    found.push(node);
                }
            }
        }
    }
    found
}

fn has_pending_children(node: &Gd<Node>) -> bool {
    node.get_child_count() > 0
        && node
            .get_child(-1)
            .is_some_and(|last| !last.is_inside_tree())
}

fn belongs<T: GodotClass + Inherits<Node>>(node: &Gd<T>, root: &Gd<SceneRoot>) -> bool {
    SceneRoot::resolve_as_parent(&node.clone().upcast::<Node>()).as_ref() == Some(root)
}
