//! Bind an exported event queue to ordered delivery after Bevy returns.
use crate::app::BevyApp;
use crate::data::config::DataTransferConfig;
use crate::export::publication::ExportPublication;
use crate::export::queues::{ExportQueue, drain_snapshots, snapshot_batch_to_dtos};
use crate::updates::dispatcher::UpdateDispatcher;
use crate::updates::occurrences::OccurrenceStream;
use crate::updates::subscriptions::{ExportValue, UpdateSubscription};
use godot::builtin::{Array, Variant};
use godot::classes::Node;
use godot::meta::ToGodot;
use godot::obj::Gd;
use godot_apis::updates::{UpdatePayload, UpdateReceiver};
use std::marker::PhantomData;
use std::rc::Rc;

struct QueueBatch<C: DataTransferConfig>(Vec<C::Snapshot>);
impl<C: DataTransferConfig> Clone for QueueBatch<C> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<C: DataTransferConfig + 'static> ExportValue for QueueBatch<C> {
    const NAME: &'static str = "queued occurrences";
}
impl<C: DataTransferConfig + 'static> UpdatePayload for QueueBatch<C> {
    type Fields = (Vec<C::Snapshot>,);
    fn into_fields(self) -> Self::Fields {
        (self.0,)
    }
    fn write_arguments(&self, arguments: &mut Array<Variant>) {
        arguments.clear();
        arguments.push(&snapshot_batch_to_dtos(self.0.clone()).to_variant());
    }
}

/// Runtime shared by every generated event queue; no event-specific behavior lives here.
pub struct QueueSource<C: DataTransferConfig + 'static> {
    app: Gd<BevyApp>,
    owner_id: i64,
    stream: Rc<OccurrenceStream<QueueBatch<C>>>,
    _publication: Rc<ExportPublication>,
    marker: PhantomData<C>,
}

impl<C: DataTransferConfig + 'static> QueueSource<C> {
    pub fn bind(mut app: Gd<BevyApp>, owner_id: i64) -> Option<Self> {
        if !app.is_instance_valid() || !app.bind().is_initialized() {
            return None;
        }
        let mut dispatcher = UpdateDispatcher::resolve(&app.clone().upcast())?;
        let stream = dispatcher.bind_mut().occurrence_stream();
        let pending = Rc::downgrade(&stream);
        let source_app = app.clone();
        // Bevy only requests publication; all draining and callbacks happen afterward.
        let publication = dispatcher.bind_mut().register_publication(false, move || {
            let Some(stream) = pending.upgrade() else {
                return;
            };
            if !source_app.is_instance_valid() || !source_app.bind().is_initialized() {
                return;
            }
            let mut source_app = source_app.clone();
            source_app.bind_mut().with_world_mut(|world| {
                let owned = world
                    .get_non_send_resource::<ExportQueue<C::Snapshot, C>>()
                    .is_some_and(|queue| queue.belongs_to(owner_id));
                if !owned {
                    return;
                }
                let values = drain_snapshots::<C>(world);
                if !values.is_empty() {
                    stream.push(QueueBatch::<C>(values));
                }
            });
        });
        let mut bound = false;
        app.bind_mut().with_world_mut(|world| {
            if let Some(mut queue) =
                world.get_non_send_resource_mut::<ExportQueue<C::Snapshot, C>>()
            {
                queue.set_publication(&publication, owner_id);
                bound = true;
            }
        });
        bound.then_some(Self {
            app,
            owner_id,
            stream,
            _publication: publication,
            marker: PhantomData,
        })
    }

    pub fn subscribe(
        &self,
        owner: Gd<Node>,
        receiver: Gd<UpdateReceiver>,
    ) -> Option<Gd<UpdateSubscription>> {
        self.stream.subscribe(owner, receiver)
    }

    pub fn is_attached(&self) -> bool {
        if !self.app.is_instance_valid() || !self.app.bind().is_initialized() {
            return false;
        }
        let mut app = self.app.clone();
        let mut attached = false;
        app.bind_mut().with_world_mut(|world| {
            attached = world
                .get_non_send_resource::<ExportQueue<C::Snapshot, C>>()
                .is_some_and(|queue| queue.belongs_to(self.owner_id));
        });
        attached
    }
}

impl<C: DataTransferConfig + 'static> Drop for QueueSource<C> {
    fn drop(&mut self) {
        self.stream.clear();
    }
}
