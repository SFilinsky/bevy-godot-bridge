//! The same nine operations for Rust and GDScript callers.
use godot::classes::Node;

use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FormatResult};

use godot::classes::{GDScript, RefCounted};
use godot::prelude::*;

use super::storage::{Bucket, ClassKey};

/// Finds registered nodes without searching the scene's descendants.
///
/// Call the static methods; do not create a registry yourself. Each owner holds
/// its own private index, containing instance IDs rather than strong node references.
#[derive(GodotClass)]
#[class(init, base=RefCounted)]
pub struct NodeRegistry {
    pub(super) bucket_map: HashMap<ClassKey, Bucket>,
    pub(super) native_key_map: HashMap<String, Vec<ClassKey>>,
    pub(super) native_matcher: Option<Gd<GDScript>>,
    base: Base<RefCounted>,
}

/// An invalid registration or lookup. A missing service is not an error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegistryError(pub(crate) String);

impl Display for RegistryError {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> FormatResult {
        formatter.write_str(&self.0)
    }
}

impl Error for RegistryError {}

use super::storage::{find_native, find_script, level, node_key, register, unregister};

impl NodeRegistry {
    /// Register one of several providers of this class in its nearest SceneRoot.
    /// Call on tree entry. Repeated registration is harmless; tree exit withdraws it.
    pub fn register_level_instance<T: GodotClass + Inherits<Node>>(
        node: &Gd<T>,
    ) -> Result<(), RegistryError> {
        let node = node.clone().upcast::<Node>();
        let owner = level(&node)?;
        register(&node, &owner, node_key(&node), false)
    }

    /// Register the only provider of this class in its nearest SceneRoot.
    /// Call on tree entry. Repeated registration is harmless; tree exit withdraws it.
    pub fn register_level_singleton<T: GodotClass + Inherits<Node>>(
        node: &Gd<T>,
    ) -> Result<(), RegistryError> {
        let node = node.clone().upcast::<Node>();
        let owner = level(&node)?;
        register(&node, &owner, node_key(&node), true)
    }

    /// Register one of several providers of this class under the explicit owner.
    /// Call on tree entry. Repeated registration is harmless; tree exit withdraws it.
    pub fn register_owned_instance<T: GodotClass + Inherits<Node>>(
        node: &Gd<T>,
        owner: &Gd<Node>,
    ) -> Result<(), RegistryError> {
        let node = node.clone().upcast::<Node>();

        register(&node, owner, node_key(&node), false)
    }

    /// Register the only provider of this class under the explicit owner.
    /// Call on tree entry. Repeated registration is harmless; tree exit withdraws it.
    pub fn register_owned_singleton<T: GodotClass + Inherits<Node>>(
        node: &Gd<T>,
        owner: &Gd<Node>,
    ) -> Result<(), RegistryError> {
        let node = node.clone().upcast::<Node>();

        register(&node, owner, node_key(&node), true)
    }

    /// Find the singleton in the caller's nearest SceneRoot.
    /// Missing providers return None. The returned references are a snapshot.
    pub fn find_level_singleton<T: GodotClass + Inherits<Node>>(
        host: &Gd<Node>,
    ) -> Result<Option<Gd<T>>, RegistryError> {
        let owner = level(host)?;
        let node_list = find_native(&owner, &T::class_id().to_string(), true)?;
        Ok(node_list
            .into_iter()
            .next()
            .and_then(|node| node.try_cast().ok()))
    }

    /// Find all instances in the caller's nearest SceneRoot.
    /// Missing providers return an empty list. The returned references are a snapshot.
    pub fn find_level_instances<T: GodotClass + Inherits<Node>>(
        host: &Gd<Node>,
    ) -> Result<Vec<Gd<T>>, RegistryError> {
        let owner = level(host)?;
        let node_list = find_native(&owner, &T::class_id().to_string(), false)?;
        Ok(node_list
            .into_iter()
            .filter_map(|node| node.try_cast().ok())
            .collect())
    }

    /// Find the singleton under this exact owner.
    /// Missing providers return None. The returned references are a snapshot.
    pub fn find_owned_singleton<T: GodotClass + Inherits<Node>>(
        owner: &Gd<Node>,
    ) -> Result<Option<Gd<T>>, RegistryError> {
        let node_list = find_native(owner, &T::class_id().to_string(), true)?;
        Ok(node_list
            .into_iter()
            .next()
            .and_then(|node| node.try_cast().ok()))
    }

    /// Find all instances under this exact owner.
    /// Missing providers return an empty list. The returned references are a snapshot.
    pub fn find_owned_instances<T: GodotClass + Inherits<Node>>(
        owner: &Gd<Node>,
    ) -> Result<Vec<Gd<T>>, RegistryError> {
        let node_list = find_native(owner, &T::class_id().to_string(), false)?;
        Ok(node_list
            .into_iter()
            .filter_map(|node| node.try_cast().ok())
            .collect())
    }

    /// Withdraw every registration for this node. Safe to repeat; also done on tree exit.
    pub fn unregister<T: GodotClass + Inherits<Node>>(node: &Gd<T>) {
        unregister(&node.clone().upcast::<Node>());
    }
}

fn report<T: Default>(result: Result<T, RegistryError>) -> T {
    match result {
        Ok(value) => value,
        Err(error) => {
            godot_error!("NodeRegistry: {error}");
            T::default()
        }
    }
}

#[godot_api]
impl NodeRegistry {
    /// Register one of multiple providers for the nearest level.
    /// Call in _enter_tree(). Returns false and reports invalid or conflicting registration.
    #[func(rename = register_level_instance)]
    fn gd_register_level_instance(node: Gd<Node>) -> bool {
        report(Self::register_level_instance(&node).map(|()| true))
    }

    /// Register one unique provider for the nearest level.
    /// Call in _enter_tree(). Returns false and reports invalid or conflicting registration.
    #[func(rename = register_level_singleton)]
    fn gd_register_level_singleton(node: Gd<Node>) -> bool {
        report(Self::register_level_singleton(&node).map(|()| true))
    }

    /// Register one of multiple providers for an explicit owner.
    /// Call in _enter_tree(). Returns false and reports invalid or conflicting registration.
    #[func(rename = register_owned_instance)]
    fn gd_register_owned_instance(node: Gd<Node>, owner: Gd<Node>) -> bool {
        report(Self::register_owned_instance(&node, &owner).map(|()| true))
    }

    /// Register one unique provider for an explicit owner.
    /// Call in _enter_tree(). Returns false and reports invalid or conflicting registration.
    #[func(rename = register_owned_singleton)]
    fn gd_register_owned_singleton(node: Gd<Node>, owner: Gd<Node>) -> bool {
        report(Self::register_owned_singleton(&node, &owner).map(|()| true))
    }

    /// Find the singleton in the caller's level.
    /// Pass a class reference. Missing providers return null; invalid usage reports an error.
    #[func(rename = find_level_singleton)]
    fn gd_find_level_singleton(host: Gd<Node>, node_class: Variant) -> Option<Gd<Node>> {
        let result = level(&host).and_then(|owner| find_script(&owner, &node_class, true));
        report(result).into_iter().next()
    }

    /// Find all registered instances in the caller's level.
    /// Pass a class reference. Missing providers return an empty array; invalid usage reports an error.
    #[func(rename = find_level_instances)]
    fn gd_find_level_instances(host: Gd<Node>, node_class: Variant) -> Array<Gd<Node>> {
        let result = level(&host).and_then(|owner| find_script(&owner, &node_class, false));
        report(result).into_iter().collect()
    }

    /// Find the singleton under the exact owner.
    /// Pass a class reference. Missing providers return null; invalid usage reports an error.
    #[func(rename = find_owned_singleton)]
    fn gd_find_owned_singleton(owner: Gd<Node>, node_class: Variant) -> Option<Gd<Node>> {
        let result = find_script(&owner, &node_class, true);
        report(result).into_iter().next()
    }

    /// Find all registered instances under the exact owner.
    /// Pass a class reference. Missing providers return an empty array; invalid usage reports an error.
    #[func(rename = find_owned_instances)]
    fn gd_find_owned_instances(owner: Gd<Node>, node_class: Variant) -> Array<Gd<Node>> {
        let result = find_script(&owner, &node_class, false);
        report(result).into_iter().collect()
    }

    /// Withdraw all registrations now. Tree exit already does this automatically.
    #[func(rename = unregister)]
    fn gd_unregister(node: Gd<Node>) {
        Self::unregister(&node);
    }
}
