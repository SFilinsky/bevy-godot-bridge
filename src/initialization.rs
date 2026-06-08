use crate::prelude::{BevyApp, SceneRoot};
use crate::tools::collect_children;
use godot::builtin::StringName;
use godot::classes::{INode, Node};
use godot::global::{godot_error, godot_print, godot_warn};
use godot::obj::{Base, InstanceId, WithBaseField};
use godot::prelude::*;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicBool, Ordering};

const INITIALIZE_METHOD: &str = "initialize";
static DID_WARN_MISSING_COORDINATOR: AtomicBool = AtomicBool::new(false);
static IS_INITIALIZING_SCENE: AtomicBool = AtomicBool::new(false);

thread_local! {
    static PENDING_INITIALIZER_REGISTRATION_LIST:
        RefCell<Vec<(InstanceId, InitializationPhase)>> = const { RefCell::new(Vec::new()) };
}

struct InitializerRegistrationBufferGuard;

impl InitializerRegistrationBufferGuard {
    fn new() -> Self {
        IS_INITIALIZING_SCENE.store(true, Ordering::Relaxed);
        Self
    }
}

impl Drop for InitializerRegistrationBufferGuard {
    fn drop(&mut self) {
        IS_INITIALIZING_SCENE.store(false, Ordering::Relaxed);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum InitializationPhase {
    /// Godot-side setup that creates initializer nodes before import data is read.
    ///
    /// Examples: benchmark or level-design helpers that procedurally instantiate
    /// authored entity scenes.
    PreImport,

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
    fn find_for<T>(host: &Gd<T>) -> Option<Vec<Gd<InitializationCoordinator>>>
    where
        T: GodotClass + Inherits<Node>,
    {
        let host_node = host.clone().upcast::<Node>();
        let scene_root = SceneRoot::resolve_as_parent(&host_node)?;
        Some(collect_children::<InitializationCoordinator>(
            scene_root.upcast::<Node>(),
            true,
        ))
    }

    pub fn exists_for<T>(host: &Gd<T>) -> bool
    where
        T: GodotClass + Inherits<Node>,
    {
        Self::find_for(host).is_some_and(|coordinator_list| !coordinator_list.is_empty())
    }

    pub fn resolve<T>(host: &Gd<T>) -> Option<Gd<InitializationCoordinator>>
    where
        T: GodotClass + Inherits<Node>,
    {
        let host_node = host.clone().upcast::<Node>();
        if SceneRoot::resolve_as_parent(&host_node).is_none() {
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
        }

        let mut coordinator_list = Self::find_for(host).unwrap_or_default();

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
        if IS_INITIALIZING_SCENE.load(Ordering::Relaxed) {
            PENDING_INITIALIZER_REGISTRATION_LIST.with(|registration_list| {
                registration_list
                    .borrow_mut()
                    .push((initializer.instance_id(), phase));
            });
            return;
        }

        let Some(mut coordinator_list) = Self::find_for(&initializer) else {
            Self::initialize_without_coordinator(initializer);
            return;
        };

        if coordinator_list.is_empty() {
            Self::initialize_without_coordinator(initializer);
            return;
        }

        if coordinator_list.len() > 1 {
            godot_error!(
                "InitializationCoordinator::register_initializer_node() found multiple coordinators in SceneRoot."
            );
            return;
        }

        let Some(mut coordinator) = coordinator_list.pop() else {
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

    fn initialize_without_coordinator(mut initializer: Gd<Node>) {
        if !DID_WARN_MISSING_COORDINATOR.swap(true, Ordering::Relaxed) {
            godot_warn!(
                "InitializationCoordinator is missing. Registered initializer nodes will initialize immediately; add an InitializationCoordinator to coordinate startup ordering."
            );
        }

        let initialize_method = StringName::from(INITIALIZE_METHOD);
        if initializer.has_method(&initialize_method) {
            initializer.call(&initialize_method, &[]);
        }
    }

    fn resolve_scene_root(&self) -> Option<Gd<SceneRoot>> {
        let host = self.base().clone().upcast::<Node>();
        SceneRoot::resolve_as_parent(&host)
    }

    fn collect_initializers_for_phase(&self, phase: InitializationPhase) -> Vec<Gd<Node>> {
        let Some(scene_root) = self.resolve_scene_root() else {
            let path = self.base().get_path();
            godot_error!(
                "InitializationCoordinator at '{}' cannot find SceneRoot parent.",
                path
            );
            return Vec::new();
        };

        let initialize_method = StringName::from(INITIALIZE_METHOD);
        let initializer_list = collect_children::<Node>(scene_root.upcast::<Node>(), true)
            .into_iter()
            .filter(|node| {
                self.registered_initializer_id_set
                    .contains(&node.instance_id())
                    && self
                        .initializer_phase_by_id_map
                        .get(&node.instance_id())
                        .copied()
                        .unwrap_or(InitializationPhase::Entity)
                        == phase
                    && node.has_method(&initialize_method)
            })
            .collect::<Vec<_>>();

        initializer_list
    }

    fn mark_scene_initialized(&self) {
        let host = self.base().clone().upcast::<Node>();
        let Ok(mut app) = BevyApp::resolve(&host) else {
            return;
        };

        app.bind_mut().mark_scene_initialized();
    }

    fn drain_pending_initializer_registrations(&mut self) {
        PENDING_INITIALIZER_REGISTRATION_LIST.with(|registration_list| {
            for (initializer_id, phase) in registration_list.borrow_mut().drain(..) {
                self.registered_initializer_id_set.insert(initializer_id);
                self.initializer_phase_by_id_map
                    .insert(initializer_id, phase);
            }
        });
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
        let _registration_buffer_guard = InitializerRegistrationBufferGuard::new();
        // Initialization is intentionally centralized here. Individual nodes only
        // register and expose initialize(); they do not decide startup timing.
        // After this loop finishes, queued imports exist but Bevy has not ticked
        // yet. BevyApp will drain those queues on its next process callback.
        for phase in [
            InitializationPhase::PreImport,
            InitializationPhase::Configuration,
            InitializationPhase::Entity,
        ] {
            self.drain_pending_initializer_registrations();
            for mut node in self.collect_initializers_for_phase(phase) {
                initialized_count += 1;
                node.call(&initialize_method, &[]);
            }
        }
        self.drain_pending_initializer_registrations();

        godot_print!(
            "[InitializationCoordinator] initialized {} scene nodes",
            initialized_count
        );
        self.mark_scene_initialized();
    }
}

#[godot_api]
impl InitializationCoordinator {
    #[func]
    pub fn register_pre_import_initializer_node(initializer: Gd<Node>) {
        Self::register_initializer_node(initializer, InitializationPhase::PreImport);
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
