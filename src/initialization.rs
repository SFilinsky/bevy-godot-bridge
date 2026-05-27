use crate::prelude::{BevyApp, SceneRoot};
use crate::tools::collect_children;
use godot::builtin::StringName;
use godot::classes::{INode, Node};
use godot::global::{godot_error, godot_print};
use godot::obj::{Base, InstanceId, WithBaseField};
use godot::prelude::*;
use std::collections::{HashMap, HashSet};

const INITIALIZE_METHOD: &str = "initialize";

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum InitializationPhase {
    /// Settings-like data that entity startup may depend on.
    ///
    /// Examples: action static data and entity settings resources.
    Configuration,

    /// Scene-authored entities and entity component imports.
    ///
    /// These run after configuration so entity spawn logic can read startup
    /// settings before creating gameplay entities.
    Entity,
}

/// Coordinates Godot-authored startup data before Bevy starts ticking.
///
/// Initializer nodes register themselves from their own `_ready()` callbacks and
/// expose an `initialize()` method. Each registration includes the initializer
/// phase, making startup order explicit at the call site. This node calls
/// initializers once, grouped by phase and then scene-tree order, then marks the
/// hosting BevyApp as ready for its first update.
///
/// BevyApp owns the actual update loop. The coordinator must not call
/// `app.update()` or drain Bevy queues directly because initializer methods may
/// resolve and mutate BevyApp while submitting startup data.
#[derive(GodotClass)]
#[class(base=Node)]
pub struct InitializationCoordinator {
    #[export]
    #[var]
    enabled: bool,

    did_initialize: bool,
    registered_initializer_id_set: HashSet<InstanceId>,
    initializer_phase_by_id_map: HashMap<InstanceId, InitializationPhase>,

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

    pub fn register_initializer_node(initializer: Gd<Node>, phase: InitializationPhase) {
        let Some(mut coordinator) = Self::resolve(&initializer) else {
            return;
        };

        let initializer_id = initializer.instance_id();
        let mut coordinator_bind = coordinator.bind_mut();
        coordinator_bind
            .registered_initializer_id_set
            .insert(initializer_id);
        coordinator_bind
            .initializer_phase_by_id_map
            .insert(initializer_id, phase);
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
        // initializers must run before child component initializers within the
        // Entity phase.
        let initializer_list = collect_children::<Node>(scene_root.upcast::<Node>(), true)
            .into_iter()
            .filter(|node| {
                self.registered_initializer_id_set
                    .contains(&node.instance_id())
                    && node.has_method(&initialize_method)
            })
            .collect::<Vec<_>>();

        let mut indexed_initializer_list = initializer_list
            .into_iter()
            .enumerate()
            .collect::<Vec<_>>();

        indexed_initializer_list.sort_by_key(|(tree_index, node)| {
            (
                self.initializer_phase_by_id_map
                    .get(&node.instance_id())
                    .copied()
                    .unwrap_or(InitializationPhase::Entity),
                *tree_index,
            )
        });

        indexed_initializer_list
            .into_iter()
            .map(|(_tree_index, node)| node)
            .collect()
    }

    fn mark_scene_initialized(&self) {
        let host = self.base().clone().upcast::<Node>();
        let Ok(mut app) = BevyApp::resolve(&host) else {
            return;
        };

        app.bind_mut().mark_scene_initialized();
    }

    fn initialize_scene(&mut self) {
        if self.did_initialize {
            return;
        }
        self.did_initialize = true;

        if !self.enabled {
            self.mark_scene_initialized();
            return;
        }

        let initialize_method = StringName::from(INITIALIZE_METHOD);
        let mut initialized_count = 0;
        // Initialization is intentionally centralized here. Individual nodes only
        // register and expose initialize(); they do not decide startup timing.
        // After this loop finishes, queued imports exist but Bevy has not ticked
        // yet. BevyApp will drain those queues on its next process callback.
        for mut node in self.collect_initializers() {
            initialized_count += 1;
            node.call(&initialize_method, &[]);
        }

        godot_print!(
            "[InitializationCoordinator] initialized {} scene nodes",
            initialized_count
        );
        self.mark_scene_initialized();
    }
}

#[godot_api]
impl INode for InitializationCoordinator {
    fn init(base: Base<Node>) -> Self {
        Self {
            enabled: true,
            did_initialize: false,
            registered_initializer_id_set: HashSet::new(),
            initializer_phase_by_id_map: HashMap::new(),
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
