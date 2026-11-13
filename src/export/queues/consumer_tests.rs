use bevy::prelude::{App, Entity, Event, Message};
use bevy_godot4::prelude::{
    DataTransferConfig, ExportEventsPlugin, ExportMessagesPlugin, ExportQueue, IdentitySubsystem,
    bridge_dto, export_queue,
};

#[derive(Clone, Default, Message)]
struct TestNotification(u32);

#[bridge_dto(TestNotificationDto)]
struct TestNotificationSnapshot {
    #[var]
    sequence: u32,
}

struct TestNotificationTransferConfig;

struct AlternateTestNotificationTransferConfig;

#[derive(Clone, Default, Event)]
struct TestImmediateNotification(u32);

struct TestImmediateNotificationTransferConfig;

impl DataTransferConfig for TestNotificationTransferConfig {
    type DataType = TestNotification;

    type Snapshot = TestNotificationSnapshot;

    fn update_snapshot(
        value: &mut Self::Snapshot,
        data: &Self::DataType,
        _identity: &mut IdentitySubsystem,
    ) {
        value.sequence = data.0;
    }

    fn update_data(
        _value: &Self::Snapshot,
        _data: &mut Self::DataType,
        _identity: &mut IdentitySubsystem,
    ) {
    }
}

impl DataTransferConfig for AlternateTestNotificationTransferConfig {
    type DataType = TestNotification;

    type Snapshot = TestNotificationSnapshot;

    fn update_snapshot(
        value: &mut Self::Snapshot,
        data: &Self::DataType,
        _identity: &mut IdentitySubsystem,
    ) {
        value.sequence = data.0;
    }

    fn update_data(
        _value: &Self::Snapshot,
        _data: &mut Self::DataType,
        _identity: &mut IdentitySubsystem,
    ) {
    }
}

impl DataTransferConfig for TestImmediateNotificationTransferConfig {
    type DataType = TestImmediateNotification;

    type Snapshot = TestNotificationSnapshot;

    fn update_snapshot(
        value: &mut Self::Snapshot,
        data: &Self::DataType,
        _identity: &mut IdentitySubsystem,
    ) {
        value.sequence = data.0;
    }

    fn update_data(
        _value: &Self::Snapshot,
        _data: &mut Self::DataType,
        _identity: &mut IdentitySubsystem,
    ) {
    }
}

export_queue! {
    config: TestNotificationTransferConfig,
}

export_queue! {
    config: TestImmediateNotificationTransferConfig,
}

#[test]
fn export_queue_macro_compiles_for_a_bridge_consumer() {}

#[test]
fn message_export_plugin_copies_messages_into_the_shared_queue() {
    let mut app = App::new();
    app.add_plugins(ExportMessagesPlugin::<TestNotificationTransferConfig>::default());
    app.world_mut().write_message(TestNotification(10));
    app.world_mut().write_message(TestNotification(20));
    app.update();

    let queued_value_list = app
        .world_mut()
        .non_send_resource_mut::<ExportQueue<TestNotificationSnapshot, TestNotificationTransferConfig>>()
        .drain()
        .map(|notification| notification.sequence)
        .collect::<Vec<_>>();

    assert_eq!(queued_value_list, vec![10, 20]);
}

#[test]
fn message_export_plugins_keep_shared_data_types_in_separate_queues() {
    let mut app = App::new();
    app.add_plugins((
        ExportMessagesPlugin::<TestNotificationTransferConfig>::default(),
        ExportMessagesPlugin::<AlternateTestNotificationTransferConfig>::default(),
    ));
    app.world_mut().write_message(TestNotification(10));
    app.update();

    let first_queue_values = app
        .world_mut()
        .non_send_resource_mut::<ExportQueue<TestNotificationSnapshot, TestNotificationTransferConfig>>()
        .drain()
        .map(|notification| notification.sequence)
        .collect::<Vec<_>>();
    let second_queue_values = app
        .world_mut()
        .non_send_resource_mut::<ExportQueue<
            TestNotificationSnapshot,
            AlternateTestNotificationTransferConfig,
        >>()
        .drain()
        .map(|notification| notification.sequence)
        .collect::<Vec<_>>();

    assert_eq!(first_queue_values, vec![10]);
    assert_eq!(second_queue_values, vec![10]);
}

#[test]
fn event_export_plugin_copies_events_into_the_shared_queue() {
    let mut app = App::new();
    app.add_plugins(ExportEventsPlugin::<TestImmediateNotificationTransferConfig>::default());
    app.world_mut().trigger(TestImmediateNotification(10));
    app.world_mut().trigger(TestImmediateNotification(20));

    let queued_value_list = app
        .world_mut()
        .non_send_resource_mut::<ExportQueue<
            TestNotificationSnapshot,
            TestImmediateNotificationTransferConfig,
        >>()
        .drain()
        .map(|notification| notification.sequence)
        .collect::<Vec<_>>();

    assert_eq!(queued_value_list, vec![10, 20]);
}

#[derive(Clone, Event)]
struct ReferencedEntityEvent(Entity);

impl Default for ReferencedEntityEvent {
    fn default() -> Self {
        Self(Entity::PLACEHOLDER)
    }
}

struct ReferencedEntityTransferConfig;
impl DataTransferConfig for ReferencedEntityTransferConfig {
    type DataType = ReferencedEntityEvent;

    type Snapshot = TestNotificationSnapshot;
    fn update_snapshot(
        value: &mut Self::Snapshot,
        data: &Self::DataType,
        identity: &mut IdentitySubsystem,
    ) {
        value.sequence = identity.get_identity(data.0) as u32;
    }
    fn update_data(_: &Self::Snapshot, _: &mut Self::DataType, _: &mut IdentitySubsystem) {}
}

#[test]
fn an_event_keeps_its_captured_identity_after_the_source_is_despawned() {
    let mut app = App::new();
    app.add_plugins(ExportEventsPlugin::<ReferencedEntityTransferConfig>::default());
    let entity = app.world_mut().spawn_empty().id();
    app.world_mut().trigger(ReferencedEntityEvent(entity));
    app.world_mut().flush();
    app.world_mut().despawn(entity);
    let values: Vec<_> = app.world_mut()
        .non_send_resource_mut::<ExportQueue<TestNotificationSnapshot, ReferencedEntityTransferConfig>>()
        .drain().map(|value| value.sequence).collect();
    assert_eq!(values, vec![0]);
}
