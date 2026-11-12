//! Stores settings that Godot must send before Bevy uses them.

use bevy::prelude::Resource;
use std::any::type_name;

/// Stores settings that must be sent once before game code reads them.
///
/// Use this for settings edited in Godot and needed by Bevy. Reading too early,
/// or sending the settings twice, causes a clear error.
#[derive(Resource, Debug, Clone)]
pub struct RequiredSettings<T> {
    value: Option<T>,
}

impl<T> Default for RequiredSettings<T> {
    fn default() -> Self {
        Self { value: None }
    }
}

impl<T> RequiredSettings<T> {
    /// Returns whether Godot has provided the settings value.
    pub fn is_provided(&self) -> bool {
        self.value.is_some()
    }

    /// Stores the settings value once.
    pub fn set_once(&mut self, value: T) {
        if self.value.is_some() {
            let label = Self::label();
            panic!("[{label}] settings were provided more than once");
        }

        self.value = Some(value);
    }

    /// Returns the settings value or errors if Godot has not sent it yet.
    pub fn get(&self) -> &T {
        match self.value.as_ref() {
            Some(value) => value,
            None => {
                let label = Self::label();
                panic!("[{label}] required settings were used before they were provided")
            }
        }
    }

    fn label() -> &'static str {
        type_name::<T>()
    }
}
