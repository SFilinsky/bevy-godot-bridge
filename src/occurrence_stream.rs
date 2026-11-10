//! Keep every event or action result, then deliver it through the shared connections.
use crate::export_values::{Binding, ExportValue, UpdateSubscription, ValueDelivery};
use godot::builtin::Array;
use godot::classes::Node;
use godot::obj::Gd;
use godot_apis::updates::UpdateReceiver;
use std::cell::{Cell, RefCell};
use std::mem::take;
use std::rc::Rc;
observe! {
    use std::time::Instant;
    use crate::performance::layer::record_system_duration_for_current_scope;
}

/// One ordered source registered with the app's UpdateDispatcher.
pub struct OccurrenceStream<T: ExportValue> {
    pending: RefCell<Vec<T>>,
    binding_list: RefCell<Vec<Binding<T>>>,
    cleanup_pending: Rc<Cell<bool>>,
}

impl<T: ExportValue> Default for OccurrenceStream<T> {
    fn default() -> Self {
        Self {
            pending: RefCell::default(),
            binding_list: RefCell::default(),
            cleanup_pending: Rc::new(Cell::new(false)),
        }
    }
}

impl<T: ExportValue> OccurrenceStream<T> {
    pub fn push(&self, value: T) {
        self.pending.borrow_mut().push(value);
    }

    pub fn subscribe(
        self: &Rc<Self>,
        owner: Gd<Node>,
        receiver: Gd<UpdateReceiver>,
    ) -> Option<Gd<UpdateSubscription>> {
        let (binding, mut subscription) =
            Binding::subscribe(owner, None, receiver, self.cleanup_pending.clone())?;
        // A queued completion survives release of its action instance until delivery.
        subscription.bind_mut().retain_source(self.clone());
        self.binding_list.borrow_mut().push(binding);
        Some(subscription)
    }

    pub fn clear(&self) {
        self.pending.borrow_mut().clear();
        for binding in self.binding_list.borrow().iter() {
            binding.cancel();
        }
        self.binding_list.borrow_mut().clear();
    }
}

impl<T: ExportValue> ValueDelivery for OccurrenceStream<T> {
    fn pending(&self) -> bool {
        self.cleanup_pending.get() || !self.pending.borrow().is_empty()
    }
    fn pending_since(&self) -> Option<(u64, u64)> {
        None
    }
    fn subscriber_count(&self, _: i64) -> usize {
        0
    }

    fn deliver(&self, owner: &Gd<Node>) {
        observe! { let started = Instant::now(); }
        // Freeze this batch before callbacks; newly queued work waits for the next pass.
        let value_list = take(&mut *self.pending.borrow_mut());
        self.cleanup_pending.set(false);
        let binding_list = {
            let mut bindings = self.binding_list.borrow_mut();
            bindings.retain(Binding::is_active);
            bindings.clone()
        };
        let mut arguments = Array::new();
        'values: for value in value_list {
            let mut arguments_ready = false;
            for binding in &binding_list {
                if !owner.is_instance_valid() || !owner.is_inside_tree() {
                    break 'values;
                }
                binding.send(&value, &mut arguments, &mut arguments_ready);
            }
        }
        observe! {
            record_system_duration_for_current_scope(&format!("Godot occurrences: deliver {}", T::NAME), started.elapsed());
        }
    }
}
