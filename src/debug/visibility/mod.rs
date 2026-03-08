use crate::debug::debug_manager::{get_debug_flag, EDebugState};
use godot::builtin::{Array, NodePath};
use godot::classes::{INode, Node};
use godot::meta::ToGodot;
use godot::obj::{Base, WithBaseField};
use godot::prelude::{godot_api, GodotClass};

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
            last_state: None,
            base,
        }
    }

    fn ready(&mut self) {
        self.base_mut().set_process(true);
        let current_state = get_debug_flag();
        self.apply_visibility(current_state);
        self.last_state = Some(current_state);
    }

    fn process(&mut self, _delta: f64) {
        let current_state = get_debug_flag();
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
