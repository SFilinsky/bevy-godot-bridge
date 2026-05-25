use crate::prelude::SceneRoot;
use crate::tools::collect_children;
use godot::builtin::StringName;
use godot::classes::{INode, Node};
use godot::global::{godot_error, godot_print};
use godot::obj::{Base, InstanceId, WithBaseField};
use godot::prelude::*;
use std::collections::HashSet;

const INITIALIZE_METHOD: &str = "initialize";

#[derive(GodotClass)]
#[class(base=Node)]
pub struct InitializationCoordinator {
    #[export]
    #[var]
    enabled: bool,

    did_initialize: bool,
    registered_initializer_id_set: HashSet<InstanceId>,

    #[base]
    base: Base<Node>,
}

impl InitializationCoordinator {
    pub fn resolve<T>(host: &Gd<T>) -> Option<Gd<InitializationCoordinator>>
    where
        T: GodotClass + Inherits<Node>,
    {
        let host_node = host.clone().upcast::<Node>();
        let Some(scene_root) = SceneRoot::resolve_as_parent(&host_node) else {
            let host_path = if host_node.is_inside_tree() {
                host_node.get_path().to_string()
            } else {
                "<detached host>".to_string()
            };
            godot_error!(
                "InitializationCoordinator::resolve() failed for '{}': no SceneRoot parent.",
                host_path
            );
            return None;
        };

        let mut coordinator_list =
            collect_children::<InitializationCoordinator>(scene_root.upcast::<Node>(), true);

        if coordinator_list.is_empty() {
            godot_error!("InitializationCoordinator::resolve() failed: no coordinator in SceneRoot.");
            return None;
        }

        if coordinator_list.len() > 1 {
            godot_error!(
                "InitializationCoordinator::resolve() found multiple coordinators in SceneRoot."
            );
            return None;
        }

        Some(coordinator_list.remove(0))
    }

    pub fn register_initializer_node(initializer: Gd<Node>) {
        let Some(mut coordinator) = Self::resolve(&initializer) else {
            return;
        };

        coordinator
            .bind_mut()
            .registered_initializer_id_set
            .insert(initializer.instance_id());
    }

    fn resolve_scene_root(&self) -> Option<Gd<SceneRoot>> {
        let host = self.base().clone().upcast::<Node>();
        SceneRoot::resolve_as_parent(&host)
    }

    fn collect_initializers(&self) -> Vec<Gd<Node>> {
        let Some(scene_root) = self.resolve_scene_root() else {
            let path = self.base().get_path();
            godot_error!(
                "InitializationCoordinator at '{}' cannot find SceneRoot parent.",
                path
            );
            return Vec::new();
        };

        let initialize_method = StringName::from(INITIALIZE_METHOD);
        // Walk the final scene tree instead of the registration order. Parent entity
        // initializers must run before child component initializers.
        collect_children::<Node>(scene_root.upcast::<Node>(), true)
            .into_iter()
            .filter(|node| {
                self.registered_initializer_id_set
                    .contains(&node.instance_id())
                    && node.has_method(&initialize_method)
            })
            .collect()
    }

    fn initialize_scene(&mut self) {
        if self.did_initialize || !self.enabled {
            return;
        }
        self.did_initialize = true;

        let initialize_method = StringName::from(INITIALIZE_METHOD);
        let mut initialized_count = 0;
        // Initialization is intentionally centralized here. Individual nodes only
        // register and expose initialize(); they do not decide startup timing.
        for mut node in self.collect_initializers() {
            initialized_count += 1;
            node.call(&initialize_method, &[]);
        }

        godot_print!(
            "[InitializationCoordinator] initialized {} scene nodes",
            initialized_count
        );
    }
}

#[godot_api]
impl INode for InitializationCoordinator {
    fn init(base: Base<Node>) -> Self {
        Self {
            enabled: true,
            did_initialize: false,
            registered_initializer_id_set: HashSet::new(),
            base,
        }
    }

    fn process(&mut self, _delta: f64) {
        if godot::classes::Engine::singleton().is_editor_hint() {
            return;
        }

        self.initialize_scene();
    }
}
