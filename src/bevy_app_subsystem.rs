use crate::app::{BevyApp, BevyAppLookupError};
use bevy::app::{App, Plugin};
use bevy::ecs::system::SystemParam;
use bevy::prelude::NonSend;
use godot::{
    builtin::NodePath,
    classes::{Engine, Node, SceneTree},
    obj::Singleton,
    prelude::{Gd, GodotClass, Inherits},
};
use std::marker::PhantomData;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

#[derive(SystemParam)]
pub struct BevyAppSubsystem<'w, 's> {
    allocator: NonSend<'w, BevyAppIdAllocatorRef>,
    host: NonSend<'w, BevyAppHostRef>,
    phantom: PhantomData<&'s ()>,
}

impl BevyAppSubsystem<'_, '_> {
    pub fn alloc_entity_id(&mut self) -> i64 {
        self.allocator.0.fetch_add(1, Ordering::Relaxed)
    }

    pub fn host_node(&self) -> Gd<Node> {
        self.host.0.clone()
    }

    pub fn try_get_host_child<T>(&self, path: &str) -> Option<Gd<T>>
    where
        T: GodotClass + Inherits<Node>,
    {
        self.host.0.try_get_node_as::<T>(path)
    }
}

#[doc(hidden)]
#[derive(Debug)]
pub(crate) struct BevyAppIdAllocatorRef(pub(crate) Arc<AtomicI64>);

impl BevyAppIdAllocatorRef {
    pub(crate) fn new(id_counter: Arc<AtomicI64>) -> Self {
        Self(id_counter)
    }
}

#[doc(hidden)]
#[derive(Debug)]
pub(crate) struct BevyAppHostRef(pub(crate) Gd<Node>);

#[doc(hidden)]
pub(crate) struct BevyAppHostPlugin {
    host_path: String,
}

impl BevyAppHostPlugin {
    pub(crate) fn with_host(host: Gd<Node>) -> Self {
        Self {
            host_path: host.get_path().to_string(),
        }
    }
}

impl Plugin for BevyAppHostPlugin {
    fn build(&self, app: &mut App) {
        let tree = Engine::singleton()
            .get_main_loop()
            .expect("SceneTree is missing while installing BevyAppHostPlugin")
            .cast::<SceneTree>();
        let root = tree
            .get_root()
            .expect("Root node is missing while installing BevyAppHostPlugin");
        let host = root
            .get_node_or_null(&NodePath::from(self.host_path.as_str()))
            .expect("BevyAppHostPlugin failed to resolve host node by path");

        app.insert_non_send_resource(BevyAppHostRef(host));
    }
}

pub(crate) fn find_for(host: &Gd<Node>) -> Result<Gd<BevyApp>, BevyAppLookupError> {
    if let Ok(app) = find_in_parents(host) {
        return Ok(app);
    }

    let tree = host.get_tree().ok_or(BevyAppLookupError::NoSceneTree)?;
    find_singleton(&tree)
}

fn find_singleton(tree: &Gd<SceneTree>) -> Result<Gd<BevyApp>, BevyAppLookupError> {
    let root = tree.get_root().ok_or(BevyAppLookupError::NoRoot)?;

    let Some(node) = root.get_node_or_null("BevyAppSingleton") else {
        return Err(BevyAppLookupError::SingletonMissing);
    };

    node.try_cast::<BevyApp>()
        .map_err(|_| BevyAppLookupError::WrongType)
}

fn find_in_parents(start: &Gd<Node>) -> Result<Gd<BevyApp>, String> {
    let mut cur: Option<Gd<Node>> = Some(start.clone());

    while let Some(node) = cur {
        if let Ok(app) = node.clone().try_cast::<BevyApp>() {
            return Ok(app);
        }
        cur = node.get_parent();
    }

    Err("No BevyApp found in parent chain. Importer must be under a BevyApp node.".to_string())
}
