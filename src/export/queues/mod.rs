//! Shared outbound queues for values Bevy sends to Godot.
//!
//! A source plugin copies `Message` or `Event` values into an
//! [`ExportQueue`](crate::export::queues::ExportQueue).
//! The shared dispatcher publishes ordered batches through a typed class created by
//! [`export_queue!`](crate::prelude::export_queue). The queue stores Rust
//! values until Godot asks for them, so source systems do not create Godot
//! objects one item at a time.

use std::marker::PhantomData;
#[cfg(feature = "bridge-transport-profiling")]
use std::time::{Duration, Instant};
use std::vec::Drain;

use bevy::ecs::schedule::common_conditions::on_message;

use crate::export::publication::ExportPublication;
use bevy::prelude::{
    App, Event, IntoScheduleConfigs, Message, MessageReader, NonSendMut, On, Plugin, PostUpdate,
    World,
};
use godot::builtin::Array;
use godot::obj::Gd;
use std::rc::{Rc, Weak};

use crate::app::subsystem::BevyAppIdAllocatorRef;
use crate::data::config::DataTransferConfig;
use crate::data::types::TransferSnapshot;
use crate::import::subsystems::{IdentityRegistry, IdentitySubsystem};

/// Stores values that Godot has not read yet.
///
/// Each `BevyApp` owns a separate queue for every transfer configuration.
/// Producers add values in Bevy, and the typed Godot queue drains them later.
/// The queue does not know where values came from or how Godot presents them.
pub struct ExportQueue<T, C = ()> {
    value_list: Vec<T>,
    publication: Option<Weak<ExportPublication>>,
    owner_id: Option<i64>,
    config_marker: PhantomData<fn() -> C>,
}

impl<T, C> ExportQueue<T, C> {
    /// Keep repeated occurrences; only the request to publish them is coalesced.
    pub fn push(&mut self, value: T) {
        self.value_list.push(value);
        if let Some(publication) = self.publication.as_ref().and_then(Weak::upgrade) {
            publication.request();
        }
    }

    pub fn drain(&mut self) -> Drain<'_, T> {
        self.value_list.drain(..)
    }

    pub(crate) fn set_publication(&mut self, publication: &Rc<ExportPublication>, owner_id: i64) {
        self.publication = Some(Rc::downgrade(publication));
        self.owner_id = Some(owner_id);
        if !self.value_list.is_empty() {
            publication.request();
        }
    }

    pub(crate) fn belongs_to(&self, owner_id: i64) -> bool {
        self.owner_id == Some(owner_id)
    }
}

impl<T, C> Default for ExportQueue<T, C> {
    fn default() -> Self {
        Self {
            value_list: Vec::new(),
            publication: None,
            owner_id: None,
            config_marker: PhantomData,
        }
    }
}

/// Adds a scheduled collector for one Bevy `Message` type.
///
/// The collector copies messages into the app-local [`ExportQueue`]. Use the
/// same transfer config with [`export_queue!`](crate::prelude::export_queue)
/// to give Godot a typed `subscribe()` method.
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
        app.init_resource::<IdentityRegistry>()
            .init_non_send_resource::<BevyAppIdAllocatorRef>()
            .add_message::<C::DataType>()
            .init_non_send_resource::<ExportQueue<C::Snapshot, C>>()
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
/// to give Godot a typed `subscribe()` method.
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
        app.init_resource::<IdentityRegistry>()
            .init_non_send_resource::<BevyAppIdAllocatorRef>()
            .init_non_send_resource::<ExportQueue<C::Snapshot, C>>()
            .add_observer(collect_events::<C>);
    }
}

/// Removes the next batch of owned values in arrival order.
/// Rust readers use this directly; script readers create typed DTOs afterward.
pub fn drain_snapshots<C: DataTransferConfig + 'static>(world: &mut World) -> Vec<C::Snapshot> {
    let Some(mut queue) = world.get_non_send_resource_mut::<ExportQueue<C::Snapshot, C>>() else {
        return Vec::new();
    };
    queue.drain().collect()
}

/// Creates typed script values only when a Godot consumer asks for a batch.
#[doc(hidden)]
pub(crate) fn snapshot_batch_to_dtos<S: TransferSnapshot>(values: Vec<S>) -> Array<Gd<S::Dto>> {
    let mut metrics = DrainMetrics::default();
    let mut dto_list = Array::new();
    for value in values {
        let dto = metrics.measure(DrainStage::DtoCreation, || value.into_dto());
        metrics.measure(DrainStage::DtoArrayAppend, || dto_list.push(&dto));
    }
    metrics.record();
    dto_list
}

fn collect_messages<C>(
    mut message_reader: MessageReader<C::DataType>,
    mut identity: IdentitySubsystem,
    mut queue: NonSendMut<ExportQueue<C::Snapshot, C>>,
) where
    C: DataTransferConfig,
    C::DataType: Message + Clone,
{
    // Copy source values so they remain valid after Bevy clears its Messages.
    for message in message_reader.read() {
        queue.push(C::snapshot_from_data(message, &mut identity));
    }
}

fn collect_events<C>(
    event: On<C::DataType>,
    mut queue: NonSendMut<ExportQueue<C::Snapshot, C>>,
    mut identity: IdentitySubsystem,
) where
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
        let event = C::snapshot_from_data(event.event(), &mut identity);
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
    queue.push(C::snapshot_from_data(event.event(), &mut identity));
}

/// One part of a Godot queue drain that is useful to time separately.
#[derive(Clone, Copy)]
enum DrainStage {
    DtoCreation,
    DtoArrayAppend,
}

/// Combines timings for one queue drain before adding them to the profiler.
///
/// A large Event batch can create thousands of values. Recording a separate
/// timing for every value would become the most expensive part of the profile.
/// This measures the real work and adds one total for each drain stage instead.
#[derive(Default)]
struct DrainMetrics {
    #[cfg(feature = "bridge-transport-profiling")]
    dto_creation: Duration,
    #[cfg(feature = "bridge-transport-profiling")]
    dto_array_append: Duration,
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

    /// Adds combined timings to the current Bevy app's report.
    fn record(self) {
        #[cfg(feature = "bridge-transport-profiling")]
        {
            use crate::performance::layer::record_system_duration_for_current_scope;

            record_system_duration_for_current_scope(
                "bridge export queue create Godot DTOs",
                self.dto_creation,
            );
            record_system_duration_for_current_scope(
                "bridge export queue DTO array append",
                self.dto_array_append,
            );
        }
    }

    #[cfg(feature = "bridge-transport-profiling")]
    fn add_duration(&mut self, stage: DrainStage, duration: Duration) {
        match stage {
            DrainStage::DtoCreation => self.dto_creation += duration,
            DrainStage::DtoArrayAppend => self.dto_array_append += duration,
        }
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

pub(crate) mod source;
