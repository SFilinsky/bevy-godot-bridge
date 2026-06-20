use bevy::prelude::Resource;
use std::any::type_name;

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
    pub fn is_provided(&self) -> bool {
        self.value.is_some()
    }

    pub fn set_once(&mut self, value: T) {
        if self.value.is_some() {
            let label = Self::label();
            panic!("[{label}] settings were provided more than once");
        }

        self.value = Some(value);
    }

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
