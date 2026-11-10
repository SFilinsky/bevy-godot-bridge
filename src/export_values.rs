//! Read changed values once per feature, then send plain values to authored views.
//!
//! A feature supplies its reader and argument conversion. The shared batch owns
//! changed IDs, subscriptions, reusable value buffers and cancellation. No game
//! feature names or presentation rules belong here.

use std::any::{Any, TypeId};
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::mem::take;
use std::rc::{Rc, Weak};

use godot::builtin::{Array, Variant};
use godot::classes::{IRefCounted, Node, RefCounted};
use godot::global::godot_error;
use godot::obj::{Base, Gd, NewGd};
use godot::prelude::{GodotClass, godot_api};
use godot_apis::updates::{Receiver, UpdatePayload, UpdateReceiver};

use crate::entity_meta::EntityMeta;
use crate::update_dispatcher::ExportChangeSource;
observe! {
    use std::time::Instant;
    use godot::classes::{Engine, Time};
    use godot::obj::Singleton;
    use crate::performance::layer::record_system_duration_for_current_scope;
}

/// Changed IDs and the first publication time stay together until a batch takes them.
#[derive(Default)]
pub(crate) struct PendingValues {
    ids: Vec<i64>,
    since: Option<(u64, u64)>,
}

impl PendingValues {
    pub(crate) fn push(&mut self, id: i64) {
        observe! {
            if self.ids.is_empty() {
                self.since = Some((Time::singleton().get_ticks_usec(), Engine::singleton().get_process_frames()));
            }
        }
        self.ids.push(id);
    }
}

/// A reader's prepared value, gathered before any Godot callback runs.
/// Native consumers can use small copies; object readers share read-only published values.
pub trait ExportValue: UpdatePayload {
    /// Plain feature name used in profiling rows.
    const NAME: &'static str;
}

/// Cancel this view's subscription when its authored owner leaves the tree.
#[derive(GodotClass)]
#[class(base=RefCounted)]
pub struct UpdateSubscription {
    active: Rc<Cell<bool>>,
    cleanup_pending: Rc<Cell<bool>>,
    keep_alive: Option<Rc<dyn Any>>,
    base: Base<RefCounted>,
}

#[godot_api]
impl IRefCounted for UpdateSubscription {
    fn init(base: Base<RefCounted>) -> Self {
        Self {
            active: Rc::new(Cell::new(true)),
            cleanup_pending: Rc::new(Cell::new(false)),
            keep_alive: None,
            base,
        }
    }
}

#[godot_api]
impl UpdateSubscription {
    /// Safe to call repeatedly, including from another callback in this batch.
    #[func]
    pub fn cancel(&self) {
        self.active.set(false);
        self.cleanup_pending.set(true);
    }
}

impl UpdateSubscription {
    pub(crate) fn retain_source(&mut self, source: Rc<dyn Any>) {
        self.keep_alive = Some(source);
    }
}

impl Drop for UpdateSubscription {
    fn drop(&mut self) {
        self.cancel();
    }
}

#[derive(Clone)]
pub(crate) struct Binding<T: ExportValue> {
    owner: Gd<Node>,
    entity: Option<Gd<EntityMeta>>,
    consumer: Receiver<T>,
    active: Rc<Cell<bool>>,
}

impl<T: ExportValue> Binding<T> {
    pub(crate) fn cancel(&self) {
        self.active.set(false);
    }
    pub(crate) fn subscribe(
        owner: Gd<Node>,
        entity: Option<Gd<EntityMeta>>,
        receiver: Gd<UpdateReceiver>,
        cleanup_pending: Rc<Cell<bool>>,
    ) -> Option<(Self, Gd<UpdateSubscription>)> {
        // Validate destination types before retaining any subscription state.
        let consumer = match receiver.bind().resolve::<T>() {
            Ok(consumer) => consumer,
            Err(error) => {
                godot_error!("Cannot subscribe to {}: {}", T::NAME, error);
                return None;
            }
        };
        let mut subscription = UpdateSubscription::new_gd();
        subscription.bind_mut().cleanup_pending = cleanup_pending;
        let active = subscription.bind().active.clone();
        Some((
            Self {
                owner,
                entity,
                consumer,
                active,
            },
            subscription,
        ))
    }

    pub(crate) fn send(
        &self,
        value: &T,
        arguments: &mut Array<Variant>,
        arguments_ready: &mut bool,
    ) {
        // Earlier receivers may have removed a scene or cancelled this connection.
        if !self.is_active() {
            return;
        }
        match &self.consumer {
            Receiver::Callback(callback) => {
                if !*arguments_ready {
                    value.write_arguments(arguments);
                    *arguments_ready = true;
                }
                callback.callv(arguments);
            }
            Receiver::Native(receive) => receive(value.clone().into_fields()),
        }
    }

    pub(crate) fn is_active(&self) -> bool {
        self.active.get()
            && self.owner.is_instance_valid()
            && self.owner.is_inside_tree()
            && self
                .entity
                .as_ref()
                .is_none_or(|entity| entity.is_instance_valid() && entity.bind().accepts_updates())
            && self.consumer.is_valid()
    }
}

/// One reader and compact pending-value buffer per feature in one Bevy app.
pub struct ExportValueBatch<T: ExportValue> {
    cleanup_pending: Rc<Cell<bool>>,
    changed_list: Rc<RefCell<PendingValues>>,
    binding_map: RefCell<HashMap<i64, Vec<Binding<T>>>>,
    value_list: RefCell<Vec<(i64, T)>>,
    seen_set: RefCell<HashSet<i64>>,
    read: Box<dyn Fn(i64) -> Option<T>>,
}

impl<T: ExportValue> ExportValueBatch<T> {
    /// The source list is a union: changing several inputs reads an entity once.
    pub fn new(
        source_list: &[Gd<ExportChangeSource>],
        read: impl Fn(i64) -> Option<T> + 'static,
    ) -> Self {
        let changed_list = Rc::new(RefCell::new(PendingValues::default()));
        for source in source_list {
            source.clone().bind_mut().watch_native(&changed_list);
        }
        Self {
            cleanup_pending: Rc::new(Cell::new(false)),
            changed_list,
            binding_map: RefCell::new(HashMap::new()),
            value_list: RefCell::new(Vec::new()),
            seen_set: RefCell::new(HashSet::new()),
            read: Box::new(read),
        }
    }

    /// Connect either destination through the same lifetime and initial-value rules.
    pub fn subscribe(
        &self,
        owner: Gd<Node>,
        entity: Gd<EntityMeta>,
        receiver: Gd<UpdateReceiver>,
    ) -> Option<Gd<UpdateSubscription>> {
        let entity_id = entity.bind().entity_id;
        self.subscribe_for(owner, entity_id, Some(entity), receiver)
    }

    /// Level services can observe an ID without requiring a visual EntityMeta node.
    pub fn subscribe_for(
        &self,
        owner: Gd<Node>,
        entity_id: i64,
        entity: Option<Gd<EntityMeta>>,
        receiver: Gd<UpdateReceiver>,
    ) -> Option<Gd<UpdateSubscription>> {
        let (binding, subscription) =
            Binding::subscribe(owner, entity, receiver, self.cleanup_pending.clone())?;
        self.binding_map
            .borrow_mut()
            .entry(entity_id)
            .or_default()
            .push(binding);
        self.changed_list.borrow_mut().push(entity_id);
        Some(subscription)
    }
}

pub(crate) trait ValueDelivery {
    fn pending(&self) -> bool;
    fn pending_since(&self) -> Option<(u64, u64)>;
    fn subscriber_count(&self, entity_id: i64) -> usize;
    fn deliver(&self, delivery_owner: &Gd<Node>);
}

impl<T: ExportValue> ValueDelivery for ExportValueBatch<T> {
    fn pending(&self) -> bool {
        self.cleanup_pending.get() || !self.changed_list.borrow().ids.is_empty()
    }

    fn pending_since(&self) -> Option<(u64, u64)> {
        self.changed_list.borrow().since
    }
    fn subscriber_count(&self, entity_id: i64) -> usize {
        self.binding_map
            .borrow()
            .get(&entity_id)
            .map_or(0, |bindings| {
                bindings
                    .iter()
                    .filter(|binding| binding.is_active())
                    .count()
            })
    }

    fn deliver(&self, delivery_owner: &Gd<Node>) {
        observe! { let read_started = Instant::now(); }
        // Gather once before callbacks, which may change registrations or unload scenes.
        let mut changed_list = {
            let mut pending = self.changed_list.borrow_mut();
            pending.since = None;
            take(&mut pending.ids)
        };
        let mut value_list = take(&mut *self.value_list.borrow_mut());
        {
            self.cleanup_pending.set(false);
            let mut seen_set = self.seen_set.borrow_mut();
            let mut binding_map = self.binding_map.borrow_mut();
            binding_map.retain(|_, binding_list| {
                binding_list.retain(Binding::is_active);
                !binding_list.is_empty()
            });
            for entity_id in changed_list.drain(..) {
                if seen_set.insert(entity_id) && binding_map.contains_key(&entity_id) {
                    if let Some(value) = (self.read)(entity_id) {
                        value_list.push((entity_id, value));
                    }
                }
            }
            seen_set.clear();
        }
        observe! {
            record_system_duration_for_current_scope(&format!("Godot values: read {}", T::NAME), read_started.elapsed());
            let apply_started = Instant::now();
        }
        // Copy only the callbacks for one entity. Cancellation flags remain live.
        let mut callback_list = Vec::new();
        let mut argument_list = Array::new();
        for (entity_id, value) in value_list.drain(..) {
            if !delivery_owner.is_instance_valid() || !delivery_owner.is_inside_tree() {
                break;
            }
            {
                let binding_map = self.binding_map.borrow();
                if let Some(binding_list) = binding_map.get(&entity_id) {
                    callback_list.extend(
                        binding_list
                            .iter()
                            .filter(|binding| binding.is_active())
                            .cloned(),
                    );
                }
            }
            // Convert once only if at least one live Godot callback needs arguments.
            let mut arguments_ready = false;
            for binding in callback_list.drain(..) {
                if !delivery_owner.is_instance_valid() || !delivery_owner.is_inside_tree() {
                    break;
                }
                binding.send(&value, &mut argument_list, &mut arguments_ready);
            }
        }

        // Registration during callbacks may have added initial values for next frame.
        changed_list.extend(self.changed_list.borrow_mut().ids.drain(..));
        self.changed_list.borrow_mut().ids = changed_list;
        *self.value_list.borrow_mut() = value_list;
        observe! {
            record_system_duration_for_current_scope(&format!("Godot values: apply {}", T::NAME), apply_started.elapsed());
        }
    }
}

#[derive(Default)]
pub(crate) struct ValueBatchRegistry {
    typed_map: HashMap<TypeId, Rc<dyn Any>>,
    delivery_list: Vec<Weak<dyn ValueDelivery>>,
}

impl ValueBatchRegistry {
    pub(crate) fn get_or_insert<T: ExportValue>(
        &mut self,
        create: impl FnOnce() -> ExportValueBatch<T>,
    ) -> Rc<ExportValueBatch<T>> {
        let key = TypeId::of::<T>();
        if let Some(batch) = self.typed_map.get(&key) {
            return batch
                .clone()
                .downcast::<ExportValueBatch<T>>()
                .unwrap_or_else(|_| unreachable!("type key matches batch"));
        }
        let batch = Rc::new(create());
        self.typed_map.insert(key, batch.clone());
        let delivery: Rc<dyn ValueDelivery> = batch.clone();
        self.delivery_list.push(Rc::downgrade(&delivery));
        batch
    }

    pub(crate) fn pending(&self) -> bool {
        self.delivery_list
            .iter()
            .filter_map(Weak::upgrade)
            .any(|batch| batch.pending())
    }
    pub(crate) fn pending_since(&self) -> Option<(u64, u64)> {
        self.delivery_list
            .iter()
            .filter_map(Weak::upgrade)
            .filter_map(|batch| batch.pending_since())
            .min()
    }
    pub(crate) fn subscriber_count(&self, entity_id: i64) -> usize {
        self.delivery_list
            .iter()
            .filter_map(Weak::upgrade)
            .map(|batch| batch.subscriber_count(entity_id))
            .sum()
    }
    pub(crate) fn register(&mut self, delivery: Rc<dyn ValueDelivery>) {
        self.delivery_list.retain(|entry| entry.strong_count() > 0);
        self.delivery_list.push(Rc::downgrade(&delivery));
    }
    pub(crate) fn deliveries(&mut self) -> Vec<Rc<dyn ValueDelivery>> {
        self.delivery_list.retain(|entry| entry.strong_count() > 0);
        self.delivery_list
            .iter()
            .filter_map(Weak::upgrade)
            .collect()
    }
}
