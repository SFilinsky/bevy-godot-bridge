use crate::app::{BevyApp, BevyAppLookupError};
use crate::scene::scene_root::SceneRoot;
use crate::tools::collect_children;
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

pub(crate) fn resolve(host: &Gd<Node>) -> Result<Gd<BevyApp>, BevyAppLookupError> {
    if let Ok(app) = resolve_as_parent(host) {
        return Ok(app);
    }

    resolve_via_scene_root(host)
}

fn resolve_via_scene_root(host: &Gd<Node>) -> Result<Gd<BevyApp>, BevyAppLookupError> {
    let Some(scene_root) = SceneRoot::resolve_as_parent(host) else {
        return Err(BevyAppLookupError::MissingSceneRoot);
    };

    let mut apps = collect_children::<BevyApp>(scene_root.upcast::<Node>(), false);

    if apps.is_empty() {
        return Err(BevyAppLookupError::MissingUnderSceneRoot);
    }

    if apps.len() > 1 {
        return Err(BevyAppLookupError::MultipleUnderSceneRoot);
    }

    Ok(apps.remove(0))
}

fn resolve_as_parent(start: &Gd<Node>) -> Result<Gd<BevyApp>, BevyAppLookupError> {
    let mut cur: Option<Gd<Node>> = Some(start.clone());

    while let Some(node) = cur {
        if let Ok(app) = node.clone().try_cast::<BevyApp>() {
            return Ok(app);
        }
        cur = node.get_parent();
    }

    Err(BevyAppLookupError::MissingInParentChain)
}
