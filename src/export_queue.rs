//! Shared outbound queues for values Bevy sends to Godot.
//!
//! A source plugin copies `Message` or `Event` values into an
//! [`ExportQueue`](crate::export_queue::ExportQueue).
//! Godot later drains the queue through a typed class created by
//! [`export_queue!`](crate::prelude::export_queue). The queue stores Rust
//! values until Godot asks for them, so source systems do not create Godot
//! objects one item at a time.

use std::marker::PhantomData;
#[cfg(feature = "bridge-transport-profiling")]
use std::time::{Duration, Instant};

use bevy::ecs::system::SystemState;
use bevy::ecs::schedule::common_conditions::on_message;
use bevy::prelude::{
    App, Event, IntoScheduleConfigs, Message, MessageReader, NonSendMut, On, Plugin, PostUpdate,
    World,
};
use godot::builtin::{Array, Callable};
use godot::obj::Gd;
use godot::prelude::ToGodot;

use crate::dto::DataTransferConfig;
use crate::import::subsystems::IdentitySubsystem;

/// Stores values that Godot has not read yet.
///
/// Each `BevyApp` owns a separate queue for every transfer configuration.
/// Producers add values in Bevy, and the typed Godot queue drains them later.
/// The queue does not know where values came from or how Godot presents them.
pub struct ExportQueue<T, C = ()> {
    value_list: Vec<T>,
    notification_callback: Option<Callable>,
    callback_owner_id: Option<i64>,
    notification_is_scheduled: bool,
    notification_id: i64,
    config_marker: PhantomData<fn() -> C>,
}

impl<T, C> ExportQueue<T, C> {
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
        // A deferred Godot call may still be waiting after an early drain.
        // Give the next notification a new id so the old call does nothing.
        self.notification_id = self.notification_id.wrapping_add(1);
        self.value_list.drain(..)
    }

    /// Registers the Godot callback that wakes the queue's consumer.
    ///
    /// The callback runs once after the queue becomes non-empty. It is deferred
    /// so Godot drains only after the current Bevy update has finished.
    pub fn set_notification_callback(&mut self, notification_callback: Callable, owner_id: i64) {
        // A newly bound Godot queue needs its own deferred wake-up.
        self.notification_is_scheduled = false;
        self.notification_callback = Some(notification_callback);
        self.callback_owner_id = Some(owner_id);
        if !self.value_list.is_empty() {
            self.schedule_notification();
        }
    }

    /// Disconnects the Godot callback without removing pending values.
    ///
    /// A Godot queue calls this before it binds to a different Bevy app. The
    /// pending values stay in their original app until a new queue binds there.
    pub fn clear_notification_callback(&mut self, owner_id: i64) {
        if self.callback_owner_id != Some(owner_id) {
            return;
        }
        self.notification_callback = None;
        self.callback_owner_id = None;
        self.notification_is_scheduled = false;
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
        self.notification_id = self.notification_id.wrapping_add(1);
        self.notification_is_scheduled = true;
        let _ = notification_callback.call_deferred(&[self.notification_id.to_variant()]);
    }
}

impl<T, C> Default for ExportQueue<T, C> {
    fn default() -> Self {
        Self {
            value_list: Vec::new(),
            notification_callback: None,
            callback_owner_id: None,
            notification_is_scheduled: false,
            notification_id: 0,
            config_marker: PhantomData,
        }
    }
}

/// Connects one typed queue to the Godot callback that drains pending values.
///
/// Macro-generated queue classes call this while binding to a [`BevyApp`]. The
/// callback is stored without a strong Godot object reference, so it becomes
/// invalid automatically when the Godot queue is freed.
#[doc(hidden)]
pub fn set_export_queue_notification_callback<C>(
    world: &mut World,
    notification_callback: Callable,
    owner_id: i64,
)
where
    C: DataTransferConfig + 'static,
    C::DataType: 'static,
{
    let Some(mut queue) = world.get_non_send_resource_mut::<ExportQueue<C::DataType, C>>() else {
        return;
    };
    queue.set_notification_callback(notification_callback, owner_id);
}

/// Disconnects one typed Godot queue from its Bevy app.
///
/// Pending values remain in the queue. A later Godot queue can bind to the
/// same app and receive one deferred notification for that pending batch.
#[doc(hidden)]
pub fn clear_export_queue_notification_callback<C>(world: &mut World, owner_id: i64)
where
    C: DataTransferConfig + 'static,
    C::DataType: 'static,
{
    let Some(mut queue) = world.get_non_send_resource_mut::<ExportQueue<C::DataType, C>>() else {
        return;
    };
    queue.clear_notification_callback(owner_id);
}

/// Returns whether one Godot queue owns this queue's callback.
///
/// A Godot queue checks this before it drains values. A newer queue can take
/// ownership of the same Bevy queue without freeing the older Godot object.
#[doc(hidden)]
pub fn export_queue_callback_belongs_to<C>(world: &World, owner_id: i64) -> bool
where
    C: DataTransferConfig + 'static,
    C::DataType: 'static,
{
    world
        .get_non_send_resource::<ExportQueue<C::DataType, C>>()
        .is_some_and(|queue| queue.callback_owner_id == Some(owner_id))
}

/// Returns whether one deferred Godot call is still current for this queue.
///
/// A deferred callback checks this before emitting its signal. Another queue
/// can bind to the same Bevy app before that deferred call runs.
#[doc(hidden)]
pub fn export_queue_notification_is_current<C>(
    world: &World,
    owner_id: i64,
    notification_id: i64,
) -> bool
where
    C: DataTransferConfig + 'static,
    C::DataType: 'static,
{
    world
        .get_non_send_resource::<ExportQueue<C::DataType, C>>()
        .is_some_and(|queue| {
            queue.callback_owner_id == Some(owner_id)
                && queue.notification_is_scheduled
                && queue.notification_id == notification_id
        })
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
            .init_non_send_resource::<ExportQueue<C::DataType, C>>()
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
        app.init_non_send_resource::<ExportQueue<C::DataType, C>>()
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
    C: DataTransferConfig + 'static,
    C::DataType: 'static,
{
    if world
        .get_non_send_resource::<ExportQueue<C::DataType, C>>()
        .is_none()
    {
        return Array::new();
    }

    let mut metrics = DrainMetrics::default();

    // Prepare the Bevy data needed to remove this batch and create its DTOs.
    let mut state = metrics.measure(DrainStage::StateSetup, || {
        SystemState::<(NonSendMut<ExportQueue<C::DataType, C>>, IdentitySubsystem)>::new(world)
    });
    let (mut queue, mut identity, mut dto_list) = metrics.measure(DrainStage::StateAccess, || {
        let (queue, identity) = state.get_mut(world);
        (queue, identity, Array::new())
    });
    #[cfg(feature = "bridge-transport-profiling")]
    let dto_build_timing = crate::dto::OutboundDtoBuildTimingScope::begin();

    // Convert one batch at the Godot boundary instead of in every source system.
    for value in queue.drain() {
        let dto = metrics.measure(DrainStage::DtoCreation, || C::from_data(&value, &mut identity));

        // Appending to Godot's Array is separate from creating the DTO object.
        metrics.measure(DrainStage::DtoArrayAppend, || dto_list.push(&dto));
    }
    #[cfg(feature = "bridge-transport-profiling")]
    metrics.record_dto_build_timing(
        dto_build_timing.finish(),
        C::USES_DEFAULT_OUTBOUND_DTO_BUILD,
    );

    // Release Bevy's temporary borrows after the whole batch has been copied.
    metrics.measure(DrainStage::Cleanup, || state.apply(world));
    metrics.record();
    dto_list
}

fn collect_messages<C>(
    mut message_reader: MessageReader<C::DataType>,
    mut queue: NonSendMut<ExportQueue<C::DataType, C>>,
) where
    C: DataTransferConfig,
    C::DataType: Message + Clone,
{
    // Copy source values so they remain valid after Bevy clears its Messages.
    for message in message_reader.read() {
        queue.push(message.clone());
    }
}

fn collect_events<C>(event: On<C::DataType>, mut queue: NonSendMut<ExportQueue<C::DataType, C>>)
where
    C: DataTransferConfig + 'static,
    C::DataType: Event + Clone,
{
    // Copy the Event while its observer is running; Events are not retained.
    // Bevy already records this observer under its concrete Event type. Do not
    // add another span here because this function can run once for every Event.
    #[cfg(feature = "bridge-transport-profiling")]
    {
        use crate::performance::layer::record_detail_duration_for_current_scope;

        let copy_started_at = Instant::now();
        let event = event.event().clone();
        record_detail_duration_for_current_scope(
            "bridge export queue Event value copy",
            copy_started_at.elapsed(),
        );

        let push_started_at = Instant::now();
        queue.push(event);
        record_detail_duration_for_current_scope(
            "bridge export queue Event queue push",
            push_started_at.elapsed(),
        );
    }

    #[cfg(not(feature = "bridge-transport-profiling"))]
    queue.push(event.event().clone());
}

/// One part of a Godot queue drain that is useful to time separately.
#[derive(Clone, Copy)]
enum DrainStage {
    StateSetup,
    StateAccess,
    DtoCreation,
    DtoArrayAppend,
    Cleanup,
}

/// Combines timings for one queue drain before adding them to the profiler.
///
/// A large Event batch can create thousands of values. Recording a separate
/// timing for every value would become the most expensive part of the profile.
/// This measures the real work and adds one total for each drain stage instead.
#[derive(Default)]
struct DrainMetrics {
    #[cfg(feature = "bridge-transport-profiling")]
    state_setup: Duration,
    #[cfg(feature = "bridge-transport-profiling")]
    state_access: Duration,
    #[cfg(feature = "bridge-transport-profiling")]
    dto_creation_unclassified: Duration,
    #[cfg(feature = "bridge-transport-profiling")]
    dto_object_creation: Duration,
    #[cfg(feature = "bridge-transport-profiling")]
    dto_field_copy: Duration,
    #[cfg(feature = "bridge-transport-profiling")]
    default_dto_value_count: u32,
    #[cfg(feature = "bridge-transport-profiling")]
    dto_array_append: Duration,
    #[cfg(feature = "bridge-transport-profiling")]
    cleanup: Duration,
}

impl DrainMetrics {
    /// Times one piece of the drain only when transport profiling is enabled.
    #[inline]
    fn measure<T>(&mut self, stage: DrainStage, operation: impl FnOnce() -> T) -> T {
        #[cfg(feature = "bridge-transport-profiling")]
        {
            let started_at = Instant::now();
            let result = operation();
            self.add_duration(stage, started_at.elapsed());
            return result;
        }

        #[cfg(not(feature = "bridge-transport-profiling"))]
        {
            let _ = stage;
            operation()
        }
    }

    /// Adds the five combined timings to the current Bevy app's report.
    fn record(self) {
        #[cfg(feature = "bridge-transport-profiling")]
        {
            use crate::performance::layer::record_system_duration_for_current_scope;

            record_system_duration_for_current_scope(
                "bridge export queue drain state setup",
                self.state_setup,
            );
            record_system_duration_for_current_scope(
                "bridge export queue drain state access",
                self.state_access,
            );
            if self.default_dto_value_count > 0 {
                record_system_duration_for_current_scope(
                    "bridge export queue DTO object creation",
                    self.dto_object_creation,
                );
                record_system_duration_for_current_scope(
                    "bridge export queue DTO field copy",
                    self.dto_field_copy,
                );
            } else if !self.dto_creation_unclassified.is_zero() {
                record_system_duration_for_current_scope(
                    "bridge export queue DTO creation",
                    self.dto_creation_unclassified,
                );
            }
            record_system_duration_for_current_scope(
                "bridge export queue DTO array append",
                self.dto_array_append,
            );
            record_system_duration_for_current_scope(
                "bridge export queue drain cleanup",
                self.cleanup,
            );
        }
    }

    #[cfg(feature = "bridge-transport-profiling")]
    fn add_duration(&mut self, stage: DrainStage, duration: Duration) {
        match stage {
            DrainStage::StateSetup => self.state_setup += duration,
            DrainStage::StateAccess => self.state_access += duration,
            DrainStage::DtoCreation => self.dto_creation_unclassified += duration,
            DrainStage::DtoArrayAppend => self.dto_array_append += duration,
            DrainStage::Cleanup => self.cleanup += duration,
        }
    }

    #[cfg(feature = "bridge-transport-profiling")]
    fn record_dto_build_timing(
        &mut self,
        timing: crate::dto::OutboundDtoBuildTiming,
        uses_default_outbound_dto_build: bool,
    ) {
        if !uses_default_outbound_dto_build {
            return;
        }
        self.dto_object_creation = timing.object_creation;
        self.dto_field_copy = timing.field_copy;
        self.default_dto_value_count = timing.value_count;
    }

}

#[cfg(test)]
mod tests {
    use super::ExportQueue;
    use bevy::app::App;

    #[test]
    fn drain_returns_repeated_values_in_arrival_order() {
        let mut queue = ExportQueue::<i32>::default();
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
