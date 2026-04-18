use crate::debug::debug_manager::{DebugManager, EDebugState};
use godot::builtin::{Array, NodePath};
use godot::classes::{INode, Node};
use godot::meta::ToGodot;
use godot::obj::{Base, WithBaseField};
use godot::prelude::{godot_api, Gd, GodotClass};

const STATE_OFF_BIT: i64 = 1 << 0;
const STATE_COLLIDERS_BIT: i64 = 1 << 1;
const STATE_NAVMESH_BIT: i64 = 1 << 2;

#[derive(GodotClass)]
#[class(base=Node)]
pub struct DebugVisibilityGroup {
    #[export]
    target_node_list: Array<NodePath>,

    #[var(rename = visible_in_debug_modes)]
    #[export(flags = (Off = 1, Colliders = 2, Navmesh = 4))]
    visible_state_mask: i64,

    debug_manager: Option<Gd<DebugManager>>,
    last_state: Option<EDebugState>,

    #[base]
    base: Base<Node>,
}

#[godot_api]
impl INode for DebugVisibilityGroup {
    fn init(base: Base<Node>) -> Self {
        Self {
            target_node_list: Array::new(),
            visible_state_mask: STATE_OFF_BIT,
            debug_manager: None,
            last_state: None,
            base,
        }
    }

    fn ready(&mut self) {
        self.base_mut().set_process(false);

        let host = self.base().clone().upcast::<Node>();
        self.debug_manager = DebugManager::resolve(&host);

        let Some(mut manager) = self.debug_manager.as_ref().cloned() else {
            self.apply_visibility(EDebugState::Off);
            self.last_state = Some(EDebugState::Off);
            return;
        };

        let on_change = self.base().callable("_on_debug_state_changed");
        let mut manager_node = manager.clone().upcast::<Node>();
        let _ = manager_node.connect("on_debug_change", &on_change);

        let current_state = manager.bind().current_state();
        self.apply_visibility(current_state);
        self.last_state = Some(current_state);
    }
}

#[godot_api]
impl DebugVisibilityGroup {
    #[func]
    fn _on_debug_state_changed(&mut self, current_state: EDebugState) {
        if self
            .last_state
            .is_some_and(|old_state| old_state == current_state)
        {
            return;
        }

        self.apply_visibility(current_state);
        self.last_state = Some(current_state);
    }
}

impl DebugVisibilityGroup {
    fn apply_visibility(&mut self, current_state: EDebugState) {
        let is_visible = match current_state {
            EDebugState::Off => self.visible_state_mask & STATE_OFF_BIT != 0,
            EDebugState::Colliders => self.visible_state_mask & STATE_COLLIDERS_BIT != 0,
            EDebugState::Navmesh => self.visible_state_mask & STATE_NAVMESH_BIT != 0,
        };

        let mut host_node = self.base().clone();
        for path in self.target_node_list.iter_shared() {
            if let Some(mut node) = host_node.get_node_or_null(&path) {
                node.set("visible", &is_visible.to_variant());
            }
        }
    }
}
