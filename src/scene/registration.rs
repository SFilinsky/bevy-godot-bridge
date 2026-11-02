//! Use Godot's live group index to find typed services in the current tree.
use super::scene_root::SceneRoot;
use godot::classes::Node;
use godot::obj::{Base, Gd, GodotClass, Inherits};

// Group names belong to this bridge and are independent of authored node names.
fn group<T: GodotClass>() -> String {
    format!("_bevy_bridge_{}", T::class_id())
}

/// Have Godot index this provider on entry and withdraw it on exit, including reparenting.
pub fn register_service<T: GodotClass + Inherits<Node>>(base: &Base<Node>) {
    base.to_init_gd().add_to_group(&group::<T>());
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
        let mut tree = app.get_tree().expect("an entered app has a SceneTree");
        return tree
            .get_nodes_in_group(&group::<T>())
            .iter_shared()
            .filter(|node| node.get_parent().as_ref() == Some(&app))
            .filter_map(|node| node.try_cast().ok())
            .collect();
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
    let mut tree = branch.get_tree().expect("an entered root has a SceneTree");
    let mut found: Vec<_> = tree
        .get_nodes_in_group(&group::<T>())
        .iter_shared()
        .filter_map(|node| node.try_cast::<T>().ok())
        .filter(|node| belongs(node, root))
        .collect();

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
    } else {
        // Godot keeps is_node_ready() true when a previously ready subtree re-enters.
        // Later siblings are already parented but are not in the live index yet.
        let mut current = Some(host.clone());
        let mut mounting = None;
        while let Some(node) = current {
            if has_pending_children(&node) {
                mounting = Some(node.clone());
            }
            if node == branch {
                break;
            }
            current = node.get_parent();
        }
        if let Some(subtree) = mounting {
            found.extend(discover::<T>(&subtree, true).into_iter().filter(|node| {
                !node.clone().upcast::<Node>().is_inside_tree() && belongs(node, root)
            }));
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
