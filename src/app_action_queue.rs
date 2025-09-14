use bevy::app::App;

pub trait AppAction: 'static {
    fn apply(self: Box<Self>, app: &mut App);
}

#[derive(Default)]
pub struct ActionQueue {
    queue: Vec<Box<dyn AppAction>>,
}

impl ActionQueue {

    pub fn add<A: AppAction>(&mut self, action: A) {
        self.queue.push(Box::new(action));
    }

    pub fn drain(&mut self) -> Vec<Box<dyn AppAction>> {
        let pending = std::mem::take(&mut self.queue);
        return pending;
    }

}