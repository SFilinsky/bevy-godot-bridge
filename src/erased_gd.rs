use bevy::prelude::Component;
use godot::{
    classes::{Node, Object, Resource},
    obj::{Bounds, Gd, GodotClass, Inherits, InstanceId, RawGd, bounds::DynMemory},
    sys,
};

/// Lets Bevy remember a Godot node without owning it.
///
/// Use this for nodes that Godot creates and destroys. Do not make two mutable
/// references to the same Godot node from Bevy.
#[derive(Debug, Component, Clone)]
pub struct ErasedGd {
    instance_id: InstanceId,
}

impl ErasedGd {
    /// Returns the saved node as `T`, or panics if Godot already freed it.
    pub fn get<T: Inherits<Node>>(&mut self) -> Gd<T> {
        self.try_get()
            .unwrap_or_else(|| panic!("failed to get godot ref as {}", std::any::type_name::<T>()))
    }

    /// Tries to return the saved node as `T`.
    ///
    /// # Safety
    /// Do not make duplicate mutable references to the same Godot object.
    pub fn try_get<T: Inherits<Node>>(&mut self) -> Option<Gd<T>> {
        Gd::try_from_instance_id(self.instance_id).ok()
    }

    /// Saves a Godot node so Bevy can find it later.
    ///
    /// # Safety
    /// Do not make duplicate mutable references to the same Godot node. Godot
    /// nodes are not safe to change from more than one place at once.
    pub fn new<T: Inherits<Node>>(reference: Gd<T>) -> Self {
        Self {
            instance_id: reference.instance_id(),
        }
    }
}

/// Lets Bevy keep a Godot resource alive.
///
/// Unlike [`ErasedGd`], this type keeps the Godot resource alive until Bevy
/// drops it.
#[derive(Debug, bevy::prelude::Resource)]
pub struct ErasedGdResource {
    resource_id: InstanceId,
}

/// used to access raw (RawGd) object
struct Gd_<T: GodotClass> {
    raw: RawGd<T>,
}

fn maybe_inc_ref<T: GodotClass>(gd: &mut Gd<T>) {
    let gd_: &mut Gd_<T> = unsafe { std::mem::transmute(gd) };
    <Object as Bounds>::DynMemory::maybe_inc_ref(&mut gd_.raw);
}

fn maybe_inc_ref_opt<T: GodotClass>(gd: &mut Option<Gd<T>>) {
    if let Some(gd) = gd {
        let gd_: &mut Gd_<T> = unsafe { std::mem::transmute(gd) };
        <Object as Bounds>::DynMemory::maybe_inc_ref(&mut gd_.raw);
    }
}

fn maybe_dec_ref<T: GodotClass>(gd: &mut Gd<T>) -> bool {
    let gd_: &mut Gd_<T> = unsafe { std::mem::transmute(gd) };
    unsafe { <Object as Bounds>::DynMemory::maybe_dec_ref(&mut gd_.raw) }
}

impl ErasedGdResource {
    /// Returns the referenced Godot resource, panicking if it was freed unexpectedly.
    pub fn get(&mut self) -> Gd<Resource> {
        self.try_get().unwrap()
    }

    /// Attempts to return the referenced Godot resource.
    pub fn try_get(&mut self) -> Option<Gd<Resource>> {
        Gd::try_from_instance_id(self.resource_id).ok()
    }

    /// Stores a reference-counted Godot resource so Bevy can keep using it.
    pub fn new(mut reference: Gd<Resource>) -> Self {
        maybe_inc_ref(&mut reference);

        Self {
            resource_id: reference.instance_id(),
        }
    }
}

impl Clone for ErasedGdResource {
    fn clone(&self) -> Self {
        maybe_inc_ref_opt::<Resource>(&mut Gd::try_from_instance_id(self.resource_id).ok());

        Self {
            resource_id: self.resource_id,
        }
    }
}

impl Drop for ErasedGdResource {
    fn drop(&mut self) {
        let mut gd = self.get();
        let is_last = maybe_dec_ref(&mut gd); // may drop
        if is_last {
            unsafe {
                sys::interface_fn!(object_destroy)(gd.obj_sys());
            }
        }
    }
}
