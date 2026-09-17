//! Shared outbound queues for values Bevy sends to Godot.
//!
//! A source plugin copies `Message` or `Event` values into an
//! [`ExportQueue`](crate::export_queue::ExportQueue).
//! Godot later drains the queue through a typed class created by
//! [`export_queue!`](crate::prelude::export_queue). The queue stores Rust
//! values until Godot asks for them, so source systems do not create Godot
//! objects one item at a time.

use std::marker::PhantomData;

use bevy::ecs::system::SystemState;
use bevy::ecs::schedule::common_conditions::on_message;
use bevy::prelude::{
    App, Event, IntoScheduleConfigs, Message, MessageReader, NonSendMut, On, Plugin, PostUpdate,
    World,
};
use godot::builtin::{Array, Callable};
use godot::obj::Gd;

use crate::dto::DataTransferConfig;
use crate::import::subsystems::IdentitySubsystem;

/// Stores values that Godot has not read yet.
///
/// Each `BevyApp` owns a separate queue. Producers add values in Bevy, and the
/// typed Godot queue drains them later. The queue does not know where values
/// came from or how Godot presents them.
pub struct ExportQueue<T> {
    value_list: Vec<T>,
    notification_callback: Option<Callable>,
    notification_is_scheduled: bool,
}

impl<T> ExportQueue<T> {
    /// Adds one value to the end of the pending batch.
    pub fn push(&mut self, value: T) {
        let was_empty = self.value_list.is_empty();
        self.value_list.push(value);
        if was_empty {
            self.schedule_notification();
        }
    }

    /// Removes every pending value in arrival order.
    pub fn drain(&mut self) -> std::vec::Drain<'_, T> {
        self.notification_is_scheduled = false;
        self.value_list.drain(..)
    }

    /// Registers the Godot callback that wakes the queue's consumer.
    ///
    /// The callback runs once after the queue becomes non-empty. It is deferred
    /// so Godot drains only after the current Bevy update has finished.
    pub fn set_notification_callback(&mut self, notification_callback: Callable) {
        self.notification_callback = Some(notification_callback);
        if !self.value_list.is_empty() {
            self.schedule_notification();
        }
    }

    fn schedule_notification(&mut self) {
        if self.notification_is_scheduled {
            return;
        }
        let Some(notification_callback) = self.notification_callback.as_ref() else {
            return;
        };
        if !notification_callback.is_valid() {
            self.notification_callback = None;
            return;
        }

        // Wake Godot after Bevy returns control, avoiding a re-entrant world borrow.
        self.notification_is_scheduled = true;
        let _ = notification_callback.call_deferred(&[]);
    }
}

impl<T> Default for ExportQueue<T> {
    fn default() -> Self {
        Self {
            value_list: Vec::new(),
            notification_callback: None,
            notification_is_scheduled: false,
        }
    }
}

/// Connects one typed queue to the Godot callback that drains pending values.
///
/// Macro-generated queue classes call this while binding to a [`BevyApp`]. The
/// callback is stored without a strong Godot object reference, so it becomes
/// invalid automatically when the Godot queue is freed.
#[doc(hidden)]
pub fn set_export_queue_notification_callback<C>(world: &mut World, notification_callback: Callable)
where
    C: DataTransferConfig,
    C::DataType: 'static,
{
    let Some(mut queue) = world.get_non_send_resource_mut::<ExportQueue<C::DataType>>() else {
        return;
    };
    queue.set_notification_callback(notification_callback);
}

/// Adds a scheduled collector for one Bevy `Message` type.
///
/// The collector copies messages into the app-local [`ExportQueue`]. Use the
/// same transfer config with [`export_queue!`](crate::prelude::export_queue)
/// to give Godot a typed `drain()` method.
pub struct ExportMessagesPlugin<C> {
    marker: PhantomData<fn() -> C>,
}

impl<C> Default for ExportMessagesPlugin<C> {
    fn default() -> Self {
        Self {
            marker: PhantomData,
        }
    }
}

impl<C> Plugin for ExportMessagesPlugin<C>
where
    C: DataTransferConfig + 'static,
    C::DataType: Message + Clone + 'static,
{
    fn build(&self, app: &mut App) {
        app.add_message::<C::DataType>()
            .init_non_send_resource::<ExportQueue<C::DataType>>()
            .add_systems(
                PostUpdate,
                collect_messages::<C>.run_if(on_message::<C::DataType>),
            );
    }
}

/// Adds an immediate collector for one Bevy `Event` type.
///
/// The observer copies the Event before Bevy finishes triggering it. Use the
/// same transfer config with [`export_queue!`](crate::prelude::export_queue)
/// to give Godot a typed `drain()` method.
pub struct ExportEventsPlugin<C> {
    marker: PhantomData<fn() -> C>,
}

impl<C> Default for ExportEventsPlugin<C> {
    fn default() -> Self {
        Self {
            marker: PhantomData,
        }
    }
}

impl<C> Plugin for ExportEventsPlugin<C>
where
    C: DataTransferConfig + 'static,
    C::DataType: Event + Clone + 'static,
{
    fn build(&self, app: &mut App) {
        app.init_non_send_resource::<ExportQueue<C::DataType>>()
            .add_observer(collect_events::<C>);
    }
}

/// Converts and removes one full pending batch for Godot.
///
/// This is called by macro-generated Godot classes. It creates DTOs only while
/// Godot drains the queue, after Bevy has already collected the source values.
#[doc(hidden)]
pub fn drain_to_dtos<C>(world: &mut World) -> Array<Gd<C::DtoType>>
where
    C: DataTransferConfig,
    C::DataType: 'static,
{
    let _span = TransportSpan::new("bridge export queue drain");
    if world
        .get_non_send_resource::<ExportQueue<C::DataType>>()
        .is_none()
    {
        return Array::new();
    }

    let mut state = SystemState::<(NonSendMut<ExportQueue<C::DataType>>, IdentitySubsystem)>::new(world);
    let (mut queue, mut identity) = state.get_mut(world);
    let mut dto_list = Array::new();

    // Convert one batch at the Godot boundary instead of in every source system.
    for value in queue.drain() {
        let dto = C::from_data(&value, &mut identity);
        dto_list.push(&dto);
    }

    state.apply(world);
    dto_list
}

fn collect_messages<C>(
    mut message_reader: MessageReader<C::DataType>,
    mut queue: NonSendMut<ExportQueue<C::DataType>>,
) where
    C: DataTransferConfig,
    C::DataType: Message + Clone,
{
    let _span = TransportSpan::new("bridge export queue message collection");

    // Copy source values so they remain valid after Bevy clears its Messages.
    for message in message_reader.read() {
        queue.push(message.clone());
    }
}

fn collect_events<C>(event: On<C::DataType>, mut queue: NonSendMut<ExportQueue<C::DataType>>)
where
    C: DataTransferConfig,
    C::DataType: Event + Clone,
{
    let _span = TransportSpan::new("bridge export queue event collection");

    // Copy the Event while its observer is running; Events are not retained.
    queue.push(event.event().clone());
}

/// Records one bridge transport stage when profiling is enabled.
///
/// In normal builds this value is empty, so queue collection does not create a
/// tracing span or record performance data.
struct TransportSpan {
    #[cfg(feature = "bridge-transport-profiling")]
    _span: tracing::span::EnteredSpan,
}

impl TransportSpan {
    /// Starts a named transport timing when the profiling feature is enabled.
    #[inline]
    pub fn new(name: &'static str) -> Self {
        #[cfg(feature = "bridge-transport-profiling")]
        {
            return Self {
                _span: tracing::info_span!("system", name).entered(),
            };
        }

        #[cfg(not(feature = "bridge-transport-profiling"))]
        {
            let _ = name;
            Self {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::ExportQueue;
    use bevy::app::App;

    #[test]
    fn drain_returns_repeated_values_in_arrival_order() {
        let mut queue = ExportQueue::default();
        queue.push(10);
        queue.push(10);
        queue.push(20);

        assert_eq!(queue.drain().collect::<Vec<_>>(), vec![10, 10, 20]);
        assert!(queue.drain().next().is_none());
    }

    #[test]
    fn queues_belong_to_their_own_bevy_app() {
        let mut first_app = App::new();
        let mut second_app = App::new();
        first_app.init_non_send_resource::<ExportQueue<i32>>();
        second_app.init_non_send_resource::<ExportQueue<i32>>();

        first_app
            .world_mut()
            .non_send_resource_mut::<ExportQueue<i32>>()
            .push(10);
        second_app
            .world_mut()
            .non_send_resource_mut::<ExportQueue<i32>>()
            .push(20);

        assert_eq!(
            first_app
                .world_mut()
                .non_send_resource_mut::<ExportQueue<i32>>()
                .drain()
                .collect::<Vec<_>>(),
            vec![10],
        );
        assert_eq!(
            second_app
                .world_mut()
                .non_send_resource_mut::<ExportQueue<i32>>()
                .drain()
                .collect::<Vec<_>>(),
            vec![20],
        );
    }
}
