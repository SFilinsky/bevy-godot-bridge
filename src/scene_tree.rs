//! Lets Bevy systems use Godot's current `SceneTree`.

use bevy::{ecs::system::SystemParam, prelude::NonSendMut};
use godot::obj::Singleton;
use godot::{
    classes::{Engine, SceneTree},
    obj::Gd,
};
use std::marker::PhantomData;

/// Gives a Bevy system the current Godot scene tree on the main thread.
#[derive(SystemParam)]
pub struct SceneTreeSubsystem<'w, 's> {
    gd: NonSendMut<'w, SceneTreeRef>,
    phantom: PhantomData<&'s ()>,
}

impl SceneTreeSubsystem<'_, '_> {
    /// Returns the active Godot scene tree.
    pub fn get(&mut self) -> Gd<SceneTree> {
        self.gd.0.clone()
    }
}

#[doc(hidden)]
#[derive(Debug)]
pub(crate) struct SceneTreeRef(Gd<SceneTree>);

impl SceneTreeRef {
    fn get_ref() -> Gd<SceneTree> {
        Engine::singleton()
            .get_main_loop()
            .unwrap()
            .cast::<SceneTree>()
    }
}

impl Default for SceneTreeRef {
    fn default() -> Self {
        Self(Self::get_ref())
    }
}

pub(crate) mod plugins {
    use super::SceneTreeRef;
    use bevy::prelude::{App, Plugin};

    pub(crate) struct SceneTreeSubsystemPlugin;

    impl Plugin for SceneTreeSubsystemPlugin {
        fn build(&self, app: &mut App) {
            app.init_non_send_resource::<SceneTreeRef>();
        }
    }
}
