//! Declare reader inputs once; resolve stores and subscribe through the shared delivery service.
//! Mapping functions receive values only. They do not resolve nodes or apply visual effects.

use crate::dto::DataTransferConfig;
use crate::entity_meta::EntityMeta;
use crate::export_delivery::{ExportChangeSource, GodotExportDelivery};
use crate::export_snapshots::{SnapshotConfig, SnapshotPair};
use crate::export_values::{ExportValue, ExportValueBatch, ExportValueSubscription};
use godot::builtin::{Array, Callable, Variant};
use godot::classes::Node;
use godot::obj::bounds::DeclUser;
use godot::obj::{Bounds, Gd, GodotClass};
use std::marker::PhantomData;

type ReadValue<C> = <<C as DataTransferConfig>::Snapshot as SnapshotConfig<
    <C as DataTransferConfig>::DtoType,
>>::Value;

/// Generated for the common transfer config when its state node is declared.
/// Import-only configs do not need an entity store.
pub trait ExportConfig: DataTransferConfig + Sized + 'static {
    type StateNode: ExportState<Self>;
}

/// Implemented by generated state nodes; related types belong to the transfer config.
pub trait ExportState<C: DataTransferConfig>:
    GodotClass<Base = Node> + Bounds<Declarer = DeclUser>
{
    fn resolve_store(host: Gd<Node>) -> Option<Gd<Self>>;
    fn change_source(&self) -> Gd<ExportChangeSource>;
    fn read_current(&self, id: i64) -> Option<ReadValue<C>>;
    fn read_values(&self, id: i64) -> Option<SnapshotPair<ReadValue<C>>>;
    fn read_dto(&self, id: i64) -> Option<Gd<C::DtoType>>;
}

/// Skip delivery when this entity lacks the input.
pub struct Required<C>(PhantomData<C>);
/// Pass `None` when this entity lacks the input, including on removal.
pub struct Optional<C>(PhantomData<C>);
/// Read both published endpoints for a visual that smooths between updates.
pub struct RequiredPair<C>(PhantomData<C>);
/// Preserve a DTO callback even when the config also supplies native snapshots.
pub struct OptionalDto<C>(PhantomData<C>);

/// Input declarations resolve once and return values without retaining Godot borrows.
/// The outer Option means the store is gone or a required value is absent.
pub trait ReaderInputs: 'static {
    type Stores: 'static;
    type Values;
    fn resolve(host: &Gd<Node>, source_list: &mut Vec<Gd<ExportChangeSource>>) -> Self::Stores;
    fn read(stores: &Self::Stores, id: i64) -> Option<Self::Values>;
}

fn resolve_store<C>(
    host: &Gd<Node>,
    source_list: &mut Vec<Gd<ExportChangeSource>>,
) -> Gd<C::StateNode>
where
    C: ExportConfig,
{
    // Missing scene wiring is a setup error even for an optional entity capability.
    let store = C::StateNode::resolve_store(host.clone())
        .expect("reader input state node must be authored under BevyApp");
    source_list.push(store.bind().change_source());
    store
}

macro_rules! input {
    ($kind:ident, $value:ty, $read:expr) => {
        impl<C> ReaderInputs for $kind<C>
        where
            C: ExportConfig,
        {
            type Stores = Gd<C::StateNode>;
            type Values = $value;
            fn resolve(
                host: &Gd<Node>,
                source_list: &mut Vec<Gd<ExportChangeSource>>,
            ) -> Self::Stores {
                resolve_store::<C>(host, source_list)
            }
            fn read(store: &Self::Stores, id: i64) -> Option<Self::Values> {
                // A freed store is not an absent optional capability: stop this read.
                if !store.is_instance_valid() {
                    return None;
                }
                ($read)(&*store.bind(), id)
            }
        }
    };
}
input!(Required, ReadValue<C>, |store: &C::StateNode, id| store
    .read_current(id));
input!(
    Optional,
    Option<ReadValue<C>>,
    |store: &C::StateNode, id| Some(store.read_current(id))
);
input!(
    RequiredPair,
    SnapshotPair<ReadValue<C>>,
    |store: &C::StateNode, id| store.read_values(id)
);
input!(
    OptionalDto,
    Option<Gd<C::DtoType>>,
    |store: &C::StateNode, id| Some(store.read_dto(id))
);

macro_rules! inputs_tuple {
    ($($type:ident : $index:tt),+) => {
        impl<$($type: ReaderInputs),+> ReaderInputs for ($($type,)+) {
            type Stores = ($($type::Stores,)+);
            type Values = ($($type::Values,)+);
            fn resolve(host: &Gd<Node>, source_list: &mut Vec<Gd<ExportChangeSource>>) -> Self::Stores {
                ($($type::resolve(host, source_list),)+)
            }
            fn read(stores: &Self::Stores, id: i64) -> Option<Self::Values> {
                Some(($($type::read(&stores.$index, id)?,)+))
            }
        }
    };
}
inputs_tuple!(A:0);
inputs_tuple!(A:0, B:1);
inputs_tuple!(A:0, B:1, C:2);
inputs_tuple!(A:0, B:1, C:2, D:3);
inputs_tuple!(A:0, B:1, C:2, D:3, E:4);
inputs_tuple!(A:0, B:1, C:2, D:3, E:4, F:5);
inputs_tuple!(A:0, B:1, C:2, D:3, E:4, F:5, G:6);
inputs_tuple!(A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7);

/// Named function or non-capturing closure; None skips this delivery.
pub type ValueMappingFn<C> =
    fn(<<C as ReaderConfig>::Inputs as ReaderInputs>::Values) -> Option<<C as ReaderConfig>::Value>;

/// A feature's inputs and pure value mapping; shared code owns all subscription work.
pub trait ReaderConfig: 'static {
    type Inputs: ReaderInputs;
    type Value: ExportValue;
    const VALUE_MAPPING_FN: ValueMappingFn<Self>;
}

// Distinct configs must not share a batch merely because their output types match.
struct ReaderValue<C: ReaderConfig>(C::Value);
impl<C: ReaderConfig> Clone for ReaderValue<C> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<C: ReaderConfig> ExportValue for ReaderValue<C> {
    const NAME: &'static str = C::Value::NAME;
    fn write_arguments(self, argument_list: &mut Array<Variant>) {
        self.0.write_arguments(argument_list);
    }
}

fn create_batch<C: ReaderConfig>(host: Gd<Node>) -> ExportValueBatch<ReaderValue<C>> {
    // Resolve once per shared batch, then subscribe to the union of input changes.
    let mut source_list = Vec::new();
    let stores = C::Inputs::resolve(&host, &mut source_list);
    ExportValueBatch::new(&source_list, move |id| {
        // Gather copies/handles before pure mapping; no Godot borrow reaches a consumer.
        let values = C::Inputs::read(&stores, id)?;
        (C::VALUE_MAPPING_FN)(values).map(ReaderValue::<C>)
    })
}

/// Register a script consumer with the same batching and cancellation as native consumers.
pub fn watch_reader<C: ReaderConfig>(
    host: Gd<Node>,
    entity: Gd<EntityMeta>,
    callback: Callable,
) -> Gd<ExportValueSubscription> {
    let mut delivery =
        GodotExportDelivery::resolve(&host).expect("reader belongs to an authored Bevy app");
    let reader_host = host.clone();
    let batch = delivery
        .bind_mut()
        .value_batch(move || create_batch::<C>(reader_host));
    batch.watch(host, entity, callback)
}

/// Register a native receiver without converting values into script arguments.
pub fn watch_native_reader<C: ReaderConfig>(
    host: Gd<Node>,
    entity: Gd<EntityMeta>,
    receive: impl Fn(C::Value) + 'static,
) -> Gd<ExportValueSubscription> {
    let mut delivery =
        GodotExportDelivery::resolve(&host).expect("reader belongs to an authored Bevy app");
    let reader_host = host.clone();
    let batch = delivery
        .bind_mut()
        .value_batch(move || create_batch::<C>(reader_host));
    batch.watch_native(host, entity, move |value| receive(value.0))
}
