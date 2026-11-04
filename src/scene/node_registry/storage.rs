//! Owner-held storage and automatic removal. No process-global node cache.
use std::collections::HashSet;

use godot::classes::object::ConnectFlags;
use godot::classes::{GDScript, Node, Object, Script};
use godot::obj::InstanceId;
use godot::prelude::*;

use super::{NodeRegistry, RegistryError};
use crate::scene::scene_root::SceneRoot;

const INDEX: &str = "_bridge_node_registry";
const OWNERS: &str = "_bridge_registry_owners";
const CLEANUP: &str = "_bridge_registry_cleanup";

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) enum ClassKey {
    Native(String),
    Script(InstanceId),
}

pub(super) struct Bucket {
    pub singleton: bool,
    pub id_list: Vec<InstanceId>,
}

pub(super) fn node_key(node: &Gd<Node>) -> ClassKey {
    if let Some(script) = node.get_script() {
        ClassKey::Script(script.instance_id())
    } else {
        ClassKey::Native(node.get_class().to_string())
    }
}

pub(super) fn level(host: &Gd<Node>) -> Result<Gd<Node>, RegistryError> {
    validate(host)?;
    SceneRoot::resolve_as_parent(host)
        .map(Gd::upcast)
        .ok_or_else(|| {
            RegistryError(
                "Expected a SceneRoot ancestor; supply an explicit owner for previews".into(),
            )
        })
}

fn validate(node: &Gd<Node>) -> Result<(), RegistryError> {
    if !node.is_instance_valid() || !node.is_inside_tree() {
        return Err(RegistryError(
            "Registration and lookup require a live node inside the tree".into(),
        ));
    }
    Ok(())
}

fn index(owner: &Gd<Node>) -> Option<Gd<NodeRegistry>> {
    if !owner.has_meta(INDEX) {
        return None;
    }
    owner.get_meta(INDEX).try_to::<Gd<NodeRegistry>>().ok()
}

fn ensure_index(owner: &Gd<Node>) -> Gd<NodeRegistry> {
    if let Some(registry) = index(owner) {
        return registry;
    }
    let registry = NodeRegistry::new_gd();
    let mut owner = owner.clone();
    owner.set_meta(INDEX, &registry.to_variant());
    let owner_id = owner.instance_id();
    // The callback stores only an ID, so it cannot keep the owner alive.
    let callback = Callable::from_fn("clear owned registrations", move |_| {
        if let Ok(mut owner) = Gd::<Node>::try_from_instance_id(owner_id) {
            owner.remove_meta(INDEX);
        }
    });
    owner.connect_flags("tree_exiting", &callback, ConnectFlags::ONE_SHOT);
    registry
}

pub(super) fn register(
    node: &Gd<Node>,
    owner: &Gd<Node>,
    key: ClassKey,
    singleton: bool,
) -> Result<(), RegistryError> {
    // Reject ambiguous ownership before changing either node.
    validate(node)?;
    validate(owner)?;
    if node.get_tree() != owner.get_tree() {
        return Err(RegistryError(
            "Provider and owner must belong to the same SceneTree".into(),
        ));
    }
    let mut registry = ensure_index(owner);
    let mut state = registry.bind_mut();
    let id = node.instance_id();
    if !state.bucket_map.contains_key(&key) {
        state.native_key_map.clear();
    }
    if let Some(bucket) = state.bucket_map.get_mut(&key) {
        bucket.id_list.retain(|id| live_node(*id).is_some());
        if !bucket.id_list.is_empty()
            && (bucket.singleton != singleton || (singleton && bucket.id_list != [id]))
        {
            return Err(RegistryError(format!(
                "Conflicting registration for {key:?} under {}",
                owner.get_path()
            )));
        }
        if bucket.id_list.contains(&id) {
            return Ok(());
        }
    }
    let bucket = state.bucket_map.entry(key).or_insert_with(|| Bucket {
        singleton,
        id_list: Vec::new(),
    });
    bucket.singleton = singleton;
    bucket.id_list.push(id);
    drop(state);

    // Remember each owner so manual removal and tree exit use exactly the same cleanup.
    let mut node = node.clone();
    let mut owner_list = if node.has_meta(OWNERS) {
        node.get_meta(OWNERS)
            .try_to::<Array<i64>>()
            .unwrap_or_default()
    } else {
        Array::new()
    };
    let owner_id = owner.instance_id().to_i64();
    if !owner_list.contains(owner_id) {
        owner_list.push(owner_id);
    }
    node.set_meta(OWNERS, &owner_list.to_variant());
    if !node.has_meta(CLEANUP) {
        let callback = Callable::from_fn("withdraw registered node", move |_| {
            if let Ok(node) = Gd::<Node>::try_from_instance_id(id) {
                unregister(&node);
            }
        });
        node.connect("tree_exiting", &callback);
        node.set_meta(CLEANUP, &callback.to_variant());
    }
    Ok(())
}

pub(super) fn unregister(node: &Gd<Node>) {
    if !node.is_instance_valid() || !node.has_meta(OWNERS) {
        return;
    }
    let owner_list = node
        .get_meta(OWNERS)
        .try_to::<Array<i64>>()
        .unwrap_or_default();
    for owner_id in owner_list.iter_shared() {
        let Ok(owner) = Gd::<Node>::try_from_instance_id(InstanceId::from_i64(owner_id)) else {
            continue;
        };
        if let Some(mut registry) = index(&owner) {
            let mut state = registry.bind_mut();
            let previous_count = state.bucket_map.len();
            state.bucket_map.retain(|_, bucket| {
                bucket.id_list.retain(|id| *id != node.instance_id());
                !bucket.id_list.is_empty()
            });
            if state.bucket_map.len() != previous_count {
                state.native_key_map.clear();
            }
        }
    }
    if node.has_meta(OWNERS) {
        node.clone().remove_meta(OWNERS);
    }
}

fn live_node(id: InstanceId) -> Option<Gd<Node>> {
    Gd::<Node>::try_from_instance_id(id)
        .ok()
        .filter(|node| node.is_inside_tree())
}

pub(super) fn find(
    owner: &Gd<Node>,
    key: &ClassKey,
    singleton: bool,
) -> Result<Vec<Gd<Node>>, RegistryError> {
    validate(owner)?;
    let Some(registry) = index(owner) else {
        return Ok(Vec::new());
    };
    let state = registry.bind();
    let Some(bucket) = state.bucket_map.get(key) else {
        return Ok(Vec::new());
    };
    if bucket.singleton != singleton {
        return Err(RegistryError(
            "Use the lookup matching singleton or instance registration".into(),
        ));
    }
    Ok(bucket
        .id_list
        .iter()
        .filter_map(|id| live_node(*id))
        .collect())
}

pub(super) fn find_script(
    owner: &Gd<Node>,
    class: &Variant,
    singleton: bool,
) -> Result<Vec<Gd<Node>>, RegistryError> {
    validate(owner)?;
    let class_object = class.try_to::<Gd<Object>>().map_err(|_| {
        RegistryError("Pass a node class reference, not a string or instance".into())
    })?;
    if let Ok(script) = class_object.clone().try_cast::<Script>() {
        return find(owner, &ClassKey::Script(script.instance_id()), singleton);
    }
    if class_object.get_class() != "GDScriptNativeClass".into() {
        return Err(RegistryError(
            "Expected a GDScript or native node class reference".into(),
        ));
    }
    let Some(mut registry) = index(owner) else {
        return Ok(Vec::new());
    };
    // Godot does not expose the name held by GDScriptNativeClass to extensions.
    // Use the language's own type check on registered candidates, never construct a
    // dummy provider and never scan the scene. Rust typed lookup uses the index directly.
    let (mut matcher, candidate_list) = {
        let mut state = registry.bind_mut();
        let matcher = state.native_matcher.get_or_insert_with(|| {
            let mut script = GDScript::new_gd();
            script.set_source_code("extends RefCounted\nstatic func matches(node: Node, node_class: Variant) -> bool:\n\treturn is_instance_of(node, node_class)\n");
            script.reload();
            script
        }).clone();
        let candidate_list: Vec<_> = state
            .bucket_map
            .iter()
            .map(|(key, bucket)| (key.clone(), bucket.id_list.clone()))
            .collect();
        (matcher, candidate_list)
    };
    let mut result_list = Vec::new();
    let mut seen_set = HashSet::new();
    for (key, id_list) in candidate_list {
        let Some(node) = id_list.iter().find_map(|id| live_node(*id)) else {
            continue;
        };
        if matcher
            .call("matches", &[node.to_variant(), class.clone()])
            .booleanize()
        {
            for node in find(owner, &key, singleton)? {
                if seen_set.insert(node.instance_id()) {
                    result_list.push(node);
                }
            }
        }
    }
    if singleton && result_list.len() > 1 {
        return Err(RegistryError(
            "More than one registered node matches the requested singleton class".into(),
        ));
    }
    Ok(result_list)
}

pub(super) fn find_native(
    owner: &Gd<Node>,
    class: &str,
    singleton: bool,
) -> Result<Vec<Gd<Node>>, RegistryError> {
    validate(owner)?;
    let Some(mut registry) = index(owner) else {
        return Ok(Vec::new());
    };
    // Resolve class relationships once per registered type set. Repeated native
    // store lookups must not inspect every other store owned by the app.
    let key_list = {
        let mut state = registry.bind_mut();
        if let Some(key_list) = state.native_key_map.get(class) {
            key_list.clone()
        } else {
            let key_list: Vec<_> = state
                .bucket_map
                .iter()
                .filter_map(|(key, bucket)| {
                    let node = bucket.id_list.iter().find_map(|id| live_node(*id))?;
                    node.is_class(class).then(|| key.clone())
                })
                .collect();
            state
                .native_key_map
                .insert(class.to_owned(), key_list.clone());
            key_list
        }
    };
    let mut result_list = Vec::new();
    for key in key_list {
        result_list.extend(find(owner, &key, singleton)?);
    }
    if singleton && result_list.len() > 1 {
        return Err(RegistryError(
            "More than one registered node matches the requested singleton class".into(),
        ));
    }
    Ok(result_list)
}
