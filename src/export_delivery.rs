//! Deliver prepared feature values after Bevy returns.
//!
//! Readers declare their inputs; only changed IDs and initial subscriptions run.
//! The scheduler has no knowledge of game features or presentation policies.

use crate::export_values::{ExportValue, ExportValueBatch, PendingValues, ValueBatchRegistry};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use godot::builtin::StringName;
use godot::classes::{INode, IRefCounted, Node, Performance, RefCounted};
use godot::obj::{Base, Gd, Singleton, WithBaseField};
use godot::prelude::{GodotClass, godot_api};

use crate::app::BevyApp;
use crate::performance::layer::enter_app_scope;
use crate::scene::registration::{child_services, register_store_on_entry};
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
    native_list: Vec<Weak<RefCell<PendingValues>>>,
    base: Base<RefCounted>,
}

impl ExportChangeSource {
    /// Mark an actual value change, including disappearance of an optional value.
    pub fn mark(&mut self, entity_id: i64, revision: i64) {
        self.revision_map.insert(entity_id, revision);
        self.native_list.retain(|weak| {
            if let Some(pending) = weak.upgrade() {
                pending.borrow_mut().push(entity_id);
                true
            } else {
                false
            }
        });
    }

    pub(crate) fn watch_native(&mut self, pending: &Rc<RefCell<PendingValues>>) {
        self.native_list.push(Rc::downgrade(pending));
    }

    /// Forget a destroyed entity; IDs are not reused within the app.
    pub fn forget(&mut self, entity_id: i64) {
        // A feature can disappear while the entity stays alive (for example a capability).
        self.mark(entity_id, -1);
        self.revision_map.remove(&entity_id);
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
            native_list: Vec::new(),
            base,
        }
    }
}

/// One same-frame Godot delivery phase for an app, after its Bevy update returns.
#[derive(GodotClass)]
#[class(base=Node)]
pub struct GodotExportDelivery {
    value_batches: ValueBatchRegistry,
    scope_id: u64,
    last_delivery_ms: f64,
    monitor_name: Option<StringName>,
    max_delay_usec: u64,
    max_delay_frames: u64,
    base: Base<Node>,
}

impl GodotExportDelivery {
    /// Find delivery without borrowing Bevy, including while a new scene becomes ready.
    pub fn resolve(host: &Gd<Node>) -> Option<Gd<Self>> {
        let app = BevyApp::resolve(host).ok()?;
        let mut nodes = child_services::<Self>(app.upcast::<Node>());
        assert!(
            nodes.len() == 1,
            "BevyApp needs exactly one authored export delivery child"
        );
        nodes.pop()
    }
    /// Share one native value reader per feature type within this app.
    pub fn value_batch<T: ExportValue>(
        &mut self,
        create: impl FnOnce() -> ExportValueBatch<T>,
    ) -> Rc<ExportValueBatch<T>> {
        self.value_batches.get_or_insert(create)
    }
    pub(crate) fn configure(&mut self, scope_id: u64, priority: i32) {
        self.scope_id = scope_id;
        self.base_mut().set_process_priority(priority);
    }

    pub(crate) fn clear(&mut self) {
        self.value_batches = ValueBatchRegistry::default();
    }
}

#[godot_api]
impl GodotExportDelivery {
    /// Count live subscriptions when checking replacement or scene cleanup.
    #[func]
    fn subscriber_count(&self, entity_id: i64) -> i64 {
        self.value_batches.subscriber_count(entity_id) as i64
    }

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
        register_store_on_entry(&base);
        Self {
            value_batches: ValueBatchRegistry::default(),
            scope_id: 0,
            last_delivery_ms: 0.0,
            monitor_name: None,
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
    fn process(this: Gd<Self>, _delta: f64) {
        Self::deliver_pending(this);
    }
}

impl GodotExportDelivery {
    /// Run the same delivery phase from a controlled test or an explicit scheduler.
    pub fn deliver_pending(mut this: Gd<Self>) {
        // Take the published work without retaining a Rust borrow across callbacks.
        // A callback may detach its view or unload the entire owning scene.
        let (value_batch_list, scope_id) = {
            let mut owner = this.bind_mut();
            owner.last_delivery_ms = 0.0;
            if !owner.value_batches.pending() {
                return;
            }
            observe! {
                if let Some((usec, frame)) = owner.value_batches.pending_since() {
                owner.max_delay_usec = owner.max_delay_usec.max(Time::singleton().get_ticks_usec().saturating_sub(usec));
                owner.max_delay_frames = owner.max_delay_frames.max(Engine::singleton().get_process_frames().saturating_sub(frame));
                }
            }
            (owner.value_batches.deliveries(), owner.scope_id)
        };
        let _scope = enter_app_scope(scope_id);
        observe! { let started = Instant::now(); }
        if this.is_instance_valid() {
            let delivery_owner = this.clone().upcast::<Node>();
            for batch in value_batch_list {
                if !this.is_instance_valid() || !this.is_inside_tree() {
                    break;
                }
                if batch.pending() {
                    batch.deliver(&delivery_owner);
                }
            }
        }
        if this.is_instance_valid() {
            let mut owner = this.bind_mut();
            observe! {
                let duration = started.elapsed();
                owner.last_delivery_ms = duration.as_secs_f64() * 1000.0;
                record_system_duration_for_current_scope("Godot export: deliver changed features", duration);
            }
        }
    }
}
