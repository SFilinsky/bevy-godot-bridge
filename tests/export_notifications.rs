use bevy::prelude::{App, Event, Message};
use bevy_godot4::prelude::{
    DataTransferConfig, ExportEventsPlugin, ExportMessagesPlugin, ExportQueue, IdentitySubsystem,
};
use godot::obj::Base;
use godot::prelude::*;

#[derive(Clone, Default, Message)]
struct TestNotification(u32);

#[derive(GodotClass)]
#[class(init, base=RefCounted)]
struct TestNotificationDto {
    #[base]
    base: Base<RefCounted>,
}

struct TestNotificationTransferConfig;

struct AlternateTestNotificationTransferConfig;

#[derive(Clone, Default, Event)]
struct TestImmediateNotification(u32);

struct TestImmediateNotificationTransferConfig;

impl DataTransferConfig for TestNotificationTransferConfig {
    type DataType = TestNotification;
    type DtoType = TestNotificationDto;

    fn update_dto(
        _dto: &mut Gd<Self::DtoType>,
        _data: &Self::DataType,
        _identity: &mut IdentitySubsystem,
    ) {
    }

    fn update_data(
        _dto: &Gd<Self::DtoType>,
        _data: &mut Self::DataType,
        _identity: &mut IdentitySubsystem,
    ) {
    }
}

impl DataTransferConfig for AlternateTestNotificationTransferConfig {
    type DataType = TestNotification;
    type DtoType = TestNotificationDto;

    fn update_dto(
        _dto: &mut Gd<Self::DtoType>,
        _data: &Self::DataType,
        _identity: &mut IdentitySubsystem,
    ) {
    }

    fn update_data(
        _dto: &Gd<Self::DtoType>,
        _data: &mut Self::DataType,
        _identity: &mut IdentitySubsystem,
    ) {
    }
}

impl DataTransferConfig for TestImmediateNotificationTransferConfig {
    type DataType = TestImmediateNotification;
    type DtoType = TestNotificationDto;

    fn update_dto(
        _dto: &mut Gd<Self::DtoType>,
        _data: &Self::DataType,
        _identity: &mut IdentitySubsystem,
    ) {
    }

    fn update_data(
        _dto: &Gd<Self::DtoType>,
        _data: &mut Self::DataType,
        _identity: &mut IdentitySubsystem,
    ) {
    }
}

bevy_godot4::prelude::export_queue! {
    config: TestNotificationTransferConfig,
}

bevy_godot4::prelude::export_queue! {
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
        .non_send_resource_mut::<ExportQueue<TestNotification, TestNotificationTransferConfig>>()
        .drain()
        .map(|notification| notification.0)
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
        .non_send_resource_mut::<ExportQueue<TestNotification, TestNotificationTransferConfig>>()
        .drain()
        .map(|notification| notification.0)
        .collect::<Vec<_>>();
    let second_queue_values = app
        .world_mut()
        .non_send_resource_mut::<ExportQueue<
            TestNotification,
            AlternateTestNotificationTransferConfig,
        >>()
        .drain()
        .map(|notification| notification.0)
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
            TestImmediateNotification,
            TestImmediateNotificationTransferConfig,
        >>()
        .drain()
        .map(|notification| notification.0)
        .collect::<Vec<_>>();

    assert_eq!(queued_value_list, vec![10, 20]);
}
