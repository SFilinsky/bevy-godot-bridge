//! Explicit diagnostic controls; unavailable in ordinary release builds.
use crate::performance::allowed;
use godot::classes::{Node, Node3D};
use godot::obj::{Gd, NewAlloc};
use godot::prelude::{GodotClass, godot_api};
use std::cell::Cell;

#[derive(Clone, Copy, PartialEq, Default)]
enum Mode {
    #[default]
    Real,
    Compute,
    Bare,
}

thread_local! { static MODE: Cell<Mode> = const { Cell::new(Mode::Real) }; }

/// Isolate destination work without altering the production presentation policy.
#[derive(GodotClass)]
#[class(init, base=RefCounted)]
struct PresentationProbe;

#[godot_api]
impl PresentationProbe {
    /// Configure before registering destinations: 0 real, 1 no writes, 2 bare transforms.
    #[func]
    fn configure(mode: i32) {
        assert!(allowed(), "Presentation probes require a measured host");
        MODE.set(match mode {
            0 => Mode::Real,
            1 => Mode::Compute,
            2 => Mode::Bare,
            _ => panic!("Unknown presentation diagnostic mode"),
        });
    }
}

pub fn writes_enabled() -> bool {
    MODE.get() != Mode::Compute
}

pub fn target_for(mut owner: Gd<Node>, original: Gd<Node3D>) -> Gd<Node3D> {
    if MODE.get() != Mode::Bare {
        return original;
    }
    // The diagnostic destination dies with its real authored owner.
    let mut target = Node3D::new_alloc();
    target.set_name("PresentationProbeDestination");
    target.set_as_top_level(true);
    owner.add_child(&target);
    target
}
