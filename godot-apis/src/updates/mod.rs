//! Describe where an update should go, independently of who produces it.
//!
//! A source supplies typed fields and their Godot argument conversion. The
//! dispatcher checks a native receiver's field types once during registration.
//! Native delivery then calls Rust directly; script delivery uses a Callable.
//! This package has no Bevy, entity, queue, scene lookup or animation knowledge.

use godot::builtin::{Array, Callable, Variant};
use godot::classes::RefCounted;
use godot::obj::{Base, Gd};
use godot::prelude::{GodotClass, godot_api};
use std::any::{Any, type_name};
use std::rc::Rc;

/// A source's prepared fields; Godot arguments are built only when needed.
pub trait UpdatePayload: Clone + 'static {
    type Fields: 'static;
    fn into_fields(self) -> Self::Fields;
    fn write_arguments(&self, argument_list: &mut Array<Variant>);
}

struct NativeReceiver {
    callback: Box<dyn Any>,
    field_names: &'static str,
}

impl NativeReceiver {
    fn new<T: 'static>(receive: impl Fn(T) + 'static) -> Self {
        let callback: Rc<dyn Fn(T)> = Rc::new(receive);
        Self {
            callback: Box::new(callback),
            field_names: type_name::<T>(),
        }
    }

    fn resolve<T: 'static>(&self) -> Result<Rc<dyn Fn(T)>, String> {
        // Check once at setup. No Any downcast belongs in per-update delivery.
        self.callback
            .downcast_ref::<Rc<dyn Fn(T)>>()
            .cloned()
            .ok_or_else(|| {
                format!(
                    "update source provides {}, but receiver expects {}",
                    type_name::<T>(),
                    self.field_names
                )
            })
    }
}

enum Destination {
    Callback(Callable),
    Native(NativeReceiver),
}

/// A lightweight destination description, not a node or a processing loop.
/// Retain the returned subscription; the dispatcher retains the destination.
#[derive(GodotClass)]
#[class(base=RefCounted, no_init)]
pub struct UpdateReceiver {
    destination: Destination,
    base: Base<RefCounted>,
}

#[godot_api]
impl UpdateReceiver {
    /// Forward changed values to a Godot method, including a GDScript method.
    #[func]
    pub fn callback(callback: Callable) -> Gd<Self> {
        Gd::from_init_fn(|base| Self {
            destination: Destination::Callback(callback),
            base,
        })
    }
}

impl UpdateReceiver {
    /// Expose a native service input without depending on a particular source.
    pub fn native<T: 'static>(receive: impl Fn(T) + 'static) -> Gd<Self> {
        let destination = Destination::Native(NativeReceiver::new(receive));
        Gd::from_init_fn(|base| Self { destination, base })
    }

    /// Validate before the dispatcher adds a connection or queues its first value.
    pub fn resolve<T: UpdatePayload>(&self) -> Result<Receiver<T>, String> {
        match &self.destination {
            Destination::Callback(callback) if callback.is_valid() => {
                Ok(Receiver::Callback(callback.clone()))
            }
            Destination::Callback(_) => Err("update receiver needs a valid Godot callback".into()),
            Destination::Native(native) => native.resolve().map(Receiver::Native),
        }
    }
}

/// A checked destination retained by the shared dispatcher.
pub enum Receiver<T: UpdatePayload> {
    Callback(Callable),
    Native(Rc<dyn Fn(T::Fields)>),
}

impl<T: UpdatePayload> Clone for Receiver<T> {
    fn clone(&self) -> Self {
        match self {
            Self::Callback(callback) => Self::Callback(callback.clone()),
            Self::Native(receive) => Self::Native(receive.clone()),
        }
    }
}

impl<T: UpdatePayload> Receiver<T> {
    pub fn is_valid(&self) -> bool {
        match self {
            Self::Callback(callback) => callback.is_valid(),
            Self::Native(_) => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::NativeReceiver;
    use std::cell::Cell;
    use std::rc::Rc;

    #[test]
    fn checked_receiver_keeps_its_target_after_registration_description_is_dropped() {
        let value = Rc::new(Cell::new(0));
        let target = value.clone();
        let receiver = NativeReceiver::new(move |(number,): (i32,)| target.set(number));
        let connection = receiver.resolve::<(i32,)>().unwrap();
        drop(receiver);
        connection((42,));
        assert_eq!(value.get(), 42);
    }

    #[test]
    fn incompatible_types_and_field_order_are_rejected_before_delivery() {
        let receiver = NativeReceiver::new(|_: (f32, bool)| {});
        assert!(receiver.resolve::<(f32, bool)>().is_ok());
        assert!(receiver.resolve::<(bool, f32)>().is_err());
        assert!(receiver.resolve::<(f64, bool)>().is_err());
        assert!(receiver.resolve::<(f32,)>().is_err());
    }
}
