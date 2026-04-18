use bevy::app::{App, Plugin};
use bevy::ecs::system::SystemParam;
use bevy::prelude::NonSend;
use godot::{
    builtin::NodePath,
    classes::{Engine, Node, SceneTree},
    obj::{NewAlloc, Singleton},
    prelude::{Gd, GodotClass, Inherits},
};
use std::marker::PhantomData;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

#[derive(SystemParam)]
pub struct BevyAppSubsystem<'w, 's> {
    allocator: NonSend<'w, BevyAppIdAllocatorRef>,
    host: NonSend<'w, BevyAppHostRef>,
    node_host: NonSend<'w, BevyAppNodeHostRef>,
    phantom: PhantomData<&'s ()>,
}

impl BevyAppSubsystem<'_, '_> {
    pub fn alloc_entity_id(&mut self) -> i64 {
        self.allocator.0.fetch_add(1, Ordering::Relaxed)
    }

    pub fn host_node(&self) -> Gd<Node> {
        self.host.0.clone()
    }

    pub fn node_host(&self) -> Gd<Node> {
        self.node_host.0.clone()
    }

    pub fn ensure_named_root(&mut self, root_name: &str) -> Gd<Node> {
        let mut host = self.node_host();

        if let Some(existing) = host.try_get_node_as::<Node>(root_name) {
            return existing;
        }

        let mut node = Node::new_alloc();
        node.set_name(root_name);
        host.add_child(&node);
        node
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
#[derive(Debug)]
pub(crate) struct BevyAppNodeHostRef(pub(crate) Gd<Node>);

#[doc(hidden)]
pub(crate) struct BevyAppHostPlugin {
    host_path: String,
    node_host_path: String,
}

impl BevyAppHostPlugin {
    pub(crate) fn with_host(host: Gd<Node>, node_host: Gd<Node>) -> Self {
        Self {
            host_path: host.get_path().to_string(),
            node_host_path: node_host.get_path().to_string(),
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
        let node_host = root
            .get_node_or_null(&NodePath::from(self.node_host_path.as_str()))
            .expect("BevyAppHostPlugin failed to resolve node host by path");

        app.insert_non_send_resource(BevyAppHostRef(host));
        app.insert_non_send_resource(BevyAppNodeHostRef(node_host));
    }
}
