//! Capture Godot requests now and deliver their messages on the next Bevy step.
use crate::app::BevyApp;
use crate::data::config::DataTransferConfig;
use crate::data::types::DtoOf;
use crate::data::types::TransferDto;
use crate::import::subsystems::IdentitySubsystem;
use bevy::prelude::{App, FixedPreUpdate, Message, MessageWriter, NonSendMut, Plugin};
use godot::global::godot_error;
use godot::obj::Gd;
use std::marker::PhantomData;
use std::mem::take;

/// Waiting requests belong to one configuration and one Bevy app.
struct PendingImports<C: DataTransferConfig> {
    snapshot_list: Vec<C::Snapshot>,
}

impl<C: DataTransferConfig> Default for PendingImports<C> {
    fn default() -> Self {
        Self {
            snapshot_list: Vec::new(),
        }
    }
}

/// Shared implementation behind each generated, typed Godot import queue.
pub struct ImportQueue<C: DataTransferConfig> {
    app: Option<Gd<BevyApp>>,
    pending_list: Vec<C::Snapshot>,
}

impl<C: DataTransferConfig> Default for ImportQueue<C> {
    fn default() -> Self {
        Self {
            app: None,
            pending_list: Vec::new(),
        }
    }
}

impl<C: DataTransferConfig + 'static> ImportQueue<C> {
    pub fn bind_bevy_app(&mut self, app: Gd<BevyApp>) {
        self.app = Some(app);
    }

    /// Capture at submission so later edits to a DTO cannot alter this request.
    pub fn enqueue(&mut self, dto: Gd<DtoOf<C>>) {
        self.pending_list.push(dto.bind().capture_snapshot());
    }

    pub fn enqueue_many(&mut self, dto_list: impl IntoIterator<Item = Gd<DtoOf<C>>>) {
        self.pending_list.extend(
            dto_list
                .into_iter()
                .map(|dto| dto.bind().capture_snapshot()),
        );
    }

    pub fn flush(&mut self) {
        // Leave requests waiting until there is an app to receive them.
        if self.pending_list.is_empty() {
            return;
        }
        let Some(mut app) = self.app.clone() else {
            godot_error!("Import queue flush() called before bind_bevy_app()");
            return;
        };

        // Hand the captured requests to this app without converting them yet.
        let snapshot_list = take(&mut self.pending_list);
        app.bind_mut().with_world_mut(|world| {
            world.init_non_send_resource::<PendingImports<C>>();
            world
                .non_send_resource_mut::<PendingImports<C>>()
                .snapshot_list
                .extend(snapshot_list);
        });
    }

    pub fn is_attached(&self) -> bool {
        self.app.is_some()
    }
}

/// Register delivery for one request type without generating a Bevy system.
pub struct ImportQueuePlugin<C> {
    config_marker: PhantomData<fn() -> C>,
}

impl<C> Default for ImportQueuePlugin<C> {
    fn default() -> Self {
        Self {
            config_marker: PhantomData,
        }
    }
}

impl<C> Plugin for ImportQueuePlugin<C>
where
    C: DataTransferConfig + 'static,
    C::DataType: Message,
{
    fn build(&self, app: &mut App) {
        app.add_message::<C::DataType>()
            .init_non_send_resource::<PendingImports<C>>()
            .add_systems(FixedPreUpdate, deliver_requests::<C>);
    }
}

fn deliver_requests<C: DataTransferConfig + 'static>(
    mut queue: NonSendMut<PendingImports<C>>,
    mut messages: MessageWriter<C::DataType>,
    mut identity: IdentitySubsystem,
) where
    C::DataType: Message,
{
    // Resolve entity identities when Bevy consumes each captured request.
    for snapshot in queue.snapshot_list.drain(..) {
        messages.write(C::from_snapshot(&snapshot, &mut identity));
    }
}
