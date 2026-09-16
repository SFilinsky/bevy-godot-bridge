//! Godot behavior node that toggles authored scene nodes by bridge debug mode.

use crate::debug::debug_manager::{DebugManager, EDebugState};
use godot::builtin::{Array, NodePath};
use godot::classes::{INode, Node};
use godot::global::{godot_error, Error};
use godot::meta::ToGodot;
use godot::obj::{Base, WithBaseField};
use godot::prelude::{godot_api, Gd, GodotClass};

const STATE_OFF_BIT: i64 = 1 << 0;
const STATE_COLLIDERS_BIT: i64 = 1 << 1;
const STATE_NAVMESH_BIT: i64 = 1 << 2;
const STATE_CAPTURE_FLOW_BIT: i64 = 1 << 3;
const STATE_ENEMY_BUILDING_FLOW_BIT: i64 = 1 << 4;
const DEFAULT_VISIBLE_STATE_MASK: i64 =
    STATE_OFF_BIT | STATE_CAPTURE_FLOW_BIT | STATE_ENEMY_BUILDING_FLOW_BIT;

/// Applies a selected set of debug-mode visibility rules to authored target nodes.
///
/// Configure target paths and visible modes in the Godot inspector. The node
/// reacts to `DebugManager` changes and does not poll every frame.
#[derive(GodotClass)]
#[class(base=Node)]
pub struct DebugVisibilityGroup {
    #[export]
    target_node_list: Array<NodePath>,

    #[var(rename = visible_in_debug_modes)]
    #[export(flags = (Off = 1, Colliders = 2, Navmesh = 4, CaptureFlow = 8, EnemyBuildingFlow = 16))]
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
            visible_state_mask: DEFAULT_VISIBLE_STATE_MASK,
            debug_manager: None,
            last_state: None,
            base,
        }
    }

    fn ready(&mut self) {
        self.base_mut().set_process(false);

        let host = self.base().clone().upcast::<Node>();
        self.debug_manager = DebugManager::resolve(&host);

        let Some(manager) = self.debug_manager.as_ref().cloned() else {
            self.apply_visibility(EDebugState::Off);
            self.last_state = Some(EDebugState::Off);
            return;
        };

        let on_change = self.base().callable("_on_debug_state_changed");
        let mut manager_node = manager.clone().upcast::<Node>();
        let err = manager_node.connect("on_debug_change", &on_change);
        if err != Error::OK {
            godot_error!(
                "DebugVisibilityGroup: failed to connect on_debug_change signal: {:?}",
                err
            );
        }

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
        let is_visible = is_visible_in_debug_state(self.visible_state_mask, current_state);

        let host_node = self.base().clone();
        for path in self.target_node_list.iter_shared() {
            if let Some(mut node) = host_node.get_node_or_null(&path) {
                node.set("visible", &is_visible.to_variant());
            }
        }
    }
}

fn is_visible_in_debug_state(visible_state_mask: i64, current_state: EDebugState) -> bool {
    match current_state {
        EDebugState::Off => visible_state_mask & STATE_OFF_BIT != 0,
        EDebugState::Colliders => visible_state_mask & STATE_COLLIDERS_BIT != 0,
        EDebugState::Navmesh => visible_state_mask & STATE_NAVMESH_BIT != 0,
        EDebugState::CaptureFlow => visible_state_mask & STATE_CAPTURE_FLOW_BIT != 0,
        EDebugState::EnemyBuildingFlow => visible_state_mask & STATE_ENEMY_BUILDING_FLOW_BIT != 0,
    }
}

#[cfg(test)]
mod tests {
    use super::{is_visible_in_debug_state, DEFAULT_VISIBLE_STATE_MASK};
    use crate::debug::debug_manager::EDebugState;

    #[test]
    fn default_visibility_should_keep_scene_targets_visible_in_flow_modes() {
        assert!(is_visible_in_debug_state(
            DEFAULT_VISIBLE_STATE_MASK,
            EDebugState::CaptureFlow
        ));
        assert!(is_visible_in_debug_state(
            DEFAULT_VISIBLE_STATE_MASK,
            EDebugState::EnemyBuildingFlow
        ));
    }
}
