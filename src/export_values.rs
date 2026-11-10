//! Read changed values once per feature, then send plain values to authored views.
//!
//! A feature supplies its reader and argument conversion. The shared batch owns
//! changed IDs, subscriptions, reusable value buffers and cancellation. No game
//! feature names or presentation rules belong here.

use std::any::{Any, TypeId};
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::mem::take;
use std::rc::Rc;

use godot::builtin::{Array, Callable, Variant};
use godot::classes::{IRefCounted, Node, RefCounted};
use godot::obj::{Base, Gd, NewGd};
use godot::prelude::{GodotClass, godot_api};

use crate::entity_meta::EntityMeta;
use crate::export_delivery::ExportChangeSource;
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
pub trait ExportValue: Clone + 'static {
    /// Plain feature name used in profiling rows.
    const NAME: &'static str;
    /// Fill the reused argument array expected by this feature's typed callback.
    fn write_arguments(self, argument_list: &mut Array<Variant>);
}

/// Cancel this view's subscription when its authored owner leaves the tree.
#[derive(GodotClass)]
#[class(base=RefCounted)]
pub struct ExportValueSubscription {
    active: Rc<Cell<bool>>,
    cleanup_pending: Rc<Cell<bool>>,
    base: Base<RefCounted>,
}

#[godot_api]
impl IRefCounted for ExportValueSubscription {
    fn init(base: Base<RefCounted>) -> Self {
        Self {
            active: Rc::new(Cell::new(true)),
            cleanup_pending: Rc::new(Cell::new(false)),
            base,
        }
    }
}

#[godot_api]
impl ExportValueSubscription {
    /// Safe to call repeatedly, including from another callback in this batch.
    #[func]
    pub fn cancel(&self) {
        self.active.set(false);
        self.cleanup_pending.set(true);
    }
}

impl Drop for ExportValueSubscription {
    fn drop(&mut self) {
        self.cancel();
    }
}

#[derive(Clone)]
enum Consumer<T> {
    Script(Callable),
    Native(Rc<dyn Fn(T)>),
}

impl<T> Consumer<T> {
    fn is_valid(&self) -> bool {
        match self {
            Self::Script(callback) => callback.is_valid(),
            Self::Native(_) => true,
        }
    }
}

struct Binding<T> {
    owner: Gd<Node>,
    entity: Gd<EntityMeta>,
    consumer: Consumer<T>,
    active: Rc<Cell<bool>>,
}

impl<T> Binding<T> {
    fn is_active(&self) -> bool {
        self.active.get()
            && self.owner.is_instance_valid()
            && self.owner.is_inside_tree()
            && self.entity.is_instance_valid()
            && self.entity.bind().accepts_updates()
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

    /// Register an authored view. Its initial value arrives in the next delivery.
    pub fn watch(
        &self,
        owner: Gd<Node>,
        entity: Gd<EntityMeta>,
        callback: Callable,
    ) -> Gd<ExportValueSubscription> {
        self.register(owner, entity, Consumer::Script(callback))
    }

    /// Connect typed native work with the same ordering and cancellation as script views.
    pub fn watch_native(
        &self,
        owner: Gd<Node>,
        entity: Gd<EntityMeta>,
        receive: impl Fn(T) + 'static,
    ) -> Gd<ExportValueSubscription> {
        self.register(owner, entity, Consumer::Native(Rc::new(receive)))
    }

    fn register(
        &self,
        owner: Gd<Node>,
        entity: Gd<EntityMeta>,
        consumer: Consumer<T>,
    ) -> Gd<ExportValueSubscription> {
        let mut subscription = ExportValueSubscription::new_gd();
        subscription.bind_mut().cleanup_pending = self.cleanup_pending.clone();
        let entity_id = entity.bind().entity_id;
        let binding = Binding {
            owner,
            entity,
            consumer,
            active: subscription.bind().active.clone(),
        };
        self.binding_map
            .borrow_mut()
            .entry(entity_id)
            .or_default()
            .push(binding);
        self.changed_list.borrow_mut().push(entity_id);
        subscription
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
                            .map(|binding| {
                                (
                                    binding.consumer.clone(),
                                    binding.active.clone(),
                                    binding.owner.clone(),
                                    binding.entity.clone(),
                                )
                            }),
                    );
                }
            }
            // Native-only destinations never create script arguments.
            let mut arguments_ready = false;
            for (consumer, active, owner, entity) in callback_list.drain(..) {
                if !delivery_owner.is_instance_valid() || !delivery_owner.is_inside_tree() {
                    break;
                }
                if active.get()
                    && owner.is_instance_valid()
                    && owner.is_inside_tree()
                    && entity.is_instance_valid()
                    && entity.bind().accepts_updates()
                    && consumer.is_valid()
                {
                    match consumer {
                        Consumer::Script(callback) => {
                            if !arguments_ready {
                                value.clone().write_arguments(&mut argument_list);
                                arguments_ready = true;
                            }
                            callback.callv(&argument_list);
                        }
                        Consumer::Native(receive) => receive(value.clone()),
                    }
                }
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
    delivery_list: Vec<Rc<dyn ValueDelivery>>,
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
        self.delivery_list.push(batch.clone());
        batch
    }

    pub(crate) fn pending(&self) -> bool {
        self.delivery_list.iter().any(|batch| batch.pending())
    }
    pub(crate) fn pending_since(&self) -> Option<(u64, u64)> {
        self.delivery_list
            .iter()
            .filter_map(|batch| batch.pending_since())
            .min()
    }
    pub(crate) fn subscriber_count(&self, entity_id: i64) -> usize {
        self.delivery_list
            .iter()
            .map(|batch| batch.subscriber_count(entity_id))
            .sum()
    }
    pub(crate) fn deliveries(&self) -> Vec<Rc<dyn ValueDelivery>> {
        self.delivery_list.clone()
    }
}
