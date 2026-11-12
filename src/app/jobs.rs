//! Small jobs that Godot asks a `BevyApp` to do later.

use bevy::app::App;

/// One job that changes its `BevyApp`.
///
/// The job stays with one `BevyApp`, not every scene in the game.
pub trait AppAction: 'static {
    fn apply(self: Box<Self>, app: &mut App);
}

/// A first-in, first-out list of jobs waiting for Bevy.
#[derive(Default)]
pub struct ActionQueue {
    queue: Vec<Box<dyn AppAction>>,
}

impl ActionQueue {
    /// Adds a job for this `BevyApp` to run on its next update.
    pub fn add<A: AppAction>(&mut self, action: A) {
        self.queue.push(Box::new(action));
    }

    /// Takes every pending mutation in submission order.
    pub fn drain(&mut self) -> Vec<Box<dyn AppAction>> {
        let pending = std::mem::take(&mut self.queue);
        return pending;
    }
}
