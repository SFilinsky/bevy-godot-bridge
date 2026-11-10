//! Queue changed entities until Godot runs its presentation phase.
//!
//! State stores publish version numbers. Entity scenes declare which stores
//! their listeners use, so unchanged features never enter GDScript callbacks.

use std::collections::HashMap;
use std::mem::take;

use godot::builtin::StringName;
use godot::classes::{INode, IRefCounted, Node, Performance, RefCounted};
use godot::obj::{Base, Gd, Singleton, WithBaseField};
use godot::prelude::{GodotClass, godot_api};

use crate::entity_meta::EntityMeta;
use crate::performance::layer::enter_app_scope;
observe! {
    use std::time::Instant;
    use godot::classes::{Engine, Time};
    use crate::performance::{allowed, layer::record_system_duration_for_current_scope};
}

/// Shared change versions for one exported feature, independent of its value type.
#[derive(GodotClass)]
#[class(base=RefCounted)]
pub struct ExportChangeSource {
    revision_map: HashMap<i64, i64>,
    base: Base<RefCounted>,
}

impl ExportChangeSource {
    /// Mark an actual value change, including disappearance of an optional value.
    pub fn mark(&mut self, entity_id: i64, revision: i64) {
        self.revision_map.insert(entity_id, revision);
    }

    /// Forget a destroyed entity; IDs are not reused within the app.
    pub fn forget(&mut self, entity_id: i64) {
        self.revision_map.remove(&entity_id);
    }

    pub(crate) fn changed(&self, entity_id: i64, revision: i64) -> bool {
        self.revision_map.get(&entity_id) == Some(&revision)
    }

    /// Last change, including removal; generated stores use this for optional values.
    pub fn revision_for(&self, entity_id: i64) -> i64 {
        self.revision_map.get(&entity_id).copied().unwrap_or(-1)
    }
}

#[godot_api]
impl IRefCounted for ExportChangeSource {
    fn init(base: Base<RefCounted>) -> Self {
        Self {
            revision_map: HashMap::new(),
            base,
        }
    }
}

/// One same-frame Godot delivery phase for an app, after its Bevy update returns.
#[derive(GodotClass)]
#[class(base=Node)]
pub struct GodotExportDelivery {
    pending_list: Vec<(Gd<EntityMeta>, i64)>,
    delivery_list: Vec<(Gd<EntityMeta>, i64)>,
    scope_id: u64,
    last_delivery_ms: f64,
    monitor_name: Option<StringName>,
    pending_usec: u64,
    pending_frame: u64,
    max_delay_usec: u64,
    max_delay_frames: u64,
    base: Base<Node>,
}

impl GodotExportDelivery {
    pub(crate) fn configure(&mut self, scope_id: u64, priority: i32) {
        self.scope_id = scope_id;
        self.base_mut().set_process_priority(priority);
    }

    /// Enqueue only changed entities; this does not execute their visual listeners.
    pub fn enqueue(&mut self, entity: Gd<EntityMeta>, revision: i64) {
        observe! {
            if self.pending_list.is_empty() {
                self.pending_usec = Time::singleton().get_ticks_usec();
                self.pending_frame = Engine::singleton().get_process_frames();
            }
        }
        self.pending_list.push((entity, revision));
    }

    pub(crate) fn clear(&mut self) {
        self.pending_list.clear();
        self.delivery_list.clear();
    }
}

#[godot_api]
impl GodotExportDelivery {
    #[func]
    fn _last_delivery_ms(&self) -> f64 {
        self.last_delivery_ms
    }

    /// Start a fresh delay window without changing pending visual work.
    #[func]
    fn reset_delay_measurements(&mut self) {
        self.max_delay_usec = 0;
        self.max_delay_frames = 0;
    }

    /// Longest queue wait in the measured window; available in profiling builds.
    #[func]
    fn get_max_delay_usec(&self) -> u64 {
        self.max_delay_usec
    }

    /// Frames between publication and delivery, excluding later interpolation.
    #[func]
    fn get_max_delay_frames(&self) -> u64 {
        self.max_delay_frames
    }
}

#[godot_api]
impl INode for GodotExportDelivery {
    fn init(base: Base<Node>) -> Self {
        Self {
            pending_list: Vec::new(),
            delivery_list: Vec::new(),
            scope_id: 0,
            last_delivery_ms: 0.0,
            monitor_name: None,
            pending_usec: 0,
            pending_frame: 0,
            max_delay_usec: 0,
            max_delay_frames: 0,
            base,
        }
    }

    fn enter_tree(&mut self) {
        observe! {
            if allowed() {
                let name = StringName::from(format!("Bridge/{}/Godot delivery ms", self.scope_id).as_str());
                Performance::singleton().add_custom_monitor(&name, &self.to_gd().callable("_last_delivery_ms"));
                self.monitor_name = Some(name);
            }
        }
    }

    fn exit_tree(&mut self) {
        if let Some(name) = self.monitor_name.take() {
            Performance::singleton().remove_custom_monitor(&name);
        }
        self.clear();
    }

    #[func(gd_self)]
    fn process(mut this: Gd<Self>, _delta: f64) {
        // Take the published work without retaining a Rust borrow across callbacks.
        // A callback may detach its view or unload the entire owning scene.
        let (mut delivery_list, scope_id) = {
            let mut owner = this.bind_mut();
            owner.last_delivery_ms = 0.0;
            if owner.pending_list.is_empty() {
                return;
            }
            observe! {
                owner.max_delay_usec = owner.max_delay_usec.max(Time::singleton().get_ticks_usec().saturating_sub(owner.pending_usec));
                owner.max_delay_frames = owner.max_delay_frames.max(Engine::singleton().get_process_frames().saturating_sub(owner.pending_frame));
            }
            let delivery_list = take(&mut owner.pending_list);
            owner.pending_list = take(&mut owner.delivery_list);
            (delivery_list, owner.scope_id)
        };
        let _scope = enter_app_scope(scope_id);
        observe! { let started = Instant::now(); }
        for (entity, revision) in delivery_list.drain(..) {
            if !this.is_instance_valid() || !this.is_inside_tree() {
                break;
            }
            if entity.is_instance_valid() {
                EntityMeta::_deliver_changes(entity, revision);
            }
        }
        if this.is_instance_valid() {
            let mut owner = this.bind_mut();
            owner.delivery_list = delivery_list;
            observe! {
                let duration = started.elapsed();
                owner.last_delivery_ms = duration.as_secs_f64() * 1000.0;
                record_system_duration_for_current_scope("Godot export: deliver changed features", duration);
            }
        }
    }
}
