//! Read declared component snapshots and deliver values through the shared dispatcher.
use crate::data::config::DataTransferConfig;
use crate::data::types::DtoOf;
use crate::entities::metadata::EntityMeta;
use crate::export::state::snapshots::SnapshotPair;
use crate::updates::dispatcher::{ExportChangeSource, UpdateDispatcher};
use crate::updates::subscriptions::{ExportValue, ExportValueBatch, UpdateSubscription};
use godot::builtin::{Array, Variant};
use godot::classes::Node;
#[cfg(any(debug_assertions, feature = "profiled-release"))]
use godot::global::godot_error;
use godot::meta::{GodotType, ToGodot};
use godot::obj::bounds::DeclUser;
use godot::obj::{Bounds, Gd, GodotClass};
use godot_apis::updates::{UpdatePayload, UpdateReceiver};
use std::any::type_name;
use std::marker::PhantomData;

pub trait ExportConfig: DataTransferConfig + Sized + 'static {
    type StateNode: ExportState<Self>;
}

pub trait ExportState<C: DataTransferConfig>:
    GodotClass<Base = Node> + Bounds<Declarer = DeclUser>
{
    fn resolve_store(host: Gd<Node>) -> Option<Gd<Self>>;
    fn change_source(&self) -> Gd<ExportChangeSource>;
    fn read_current(&self, id: i64) -> Option<C::Snapshot>;
    fn read_values(&self, id: i64) -> Option<SnapshotPair<C::Snapshot>>;
}

/// Precise setup failure, kept as static names so successful reads allocate nothing.
#[derive(Debug, Clone, PartialEq)]
pub struct StateReadError {
    pub component: &'static str,
    pub reason: &'static str,
}

impl StateReadError {
    pub fn missing<C: DataTransferConfig>() -> Self {
        Self {
            component: type_name::<C::DataType>(),
            reason: "required component is absent from published state",
        }
    }
}

/// Development diagnostics include the real gameplay component, not just its DTO name.
pub fn report_state_error(source: &str, id: i64, error: &StateReadError) {
    observe! { godot_error!("Cannot read {} for entity {}: {} ({})", source, id, error.component, error.reason); }
    let _ = (source, id, error);
}

pub struct Required<C>(PhantomData<C>);
pub struct Optional<C>(PhantomData<C>);
pub struct RequiredPair<C>(PhantomData<C>);

/// Read values before calling consumers, without holding a borrowed Godot node.
pub trait StateInputs: 'static {
    type Stores: 'static;
    type Values;
    fn resolve(
        host: &Gd<Node>,
        changes: &mut Vec<Gd<ExportChangeSource>>,
    ) -> Result<Self::Stores, StateReadError>;
    fn read(stores: &Self::Stores, id: i64) -> Result<Self::Values, StateReadError>;
}

/// One input's Godot representation, used for generated named one-time reads.
pub trait StateInput: StateInputs {
    type GodotValue: GodotType;
    fn godot_value(value: &Self::Values) -> Self::GodotValue;
}

fn resolve_store<C: ExportConfig>(
    host: &Gd<Node>,
    changes: &mut Vec<Gd<ExportChangeSource>>,
) -> Result<Gd<C::StateNode>, StateReadError> {
    let store = C::StateNode::resolve_store(host.clone()).ok_or(StateReadError {
        component: type_name::<C::DataType>(),
        reason: "state node is not authored under BevyApp",
    })?;
    changes.push(store.bind().change_source());
    Ok(store)
}

macro_rules! input {
    ($kind:ident, $value:ty, $read:expr) => {
        impl<C: ExportConfig> StateInputs for $kind<C> {
            type Stores = Gd<C::StateNode>;
            type Values = $value;
            fn resolve(
                host: &Gd<Node>,
                changes: &mut Vec<Gd<ExportChangeSource>>,
            ) -> Result<Self::Stores, StateReadError> {
                resolve_store::<C>(host, changes)
            }
            fn read(store: &Self::Stores, id: i64) -> Result<Self::Values, StateReadError> {
                if !store.is_instance_valid() {
                    return Err(StateReadError {
                        component: type_name::<C::DataType>(),
                        reason: "state node was freed",
                    });
                }
                ($read)(&*store.bind(), id)
            }
        }
    };
}
input!(Required, C::Snapshot, |store: &C::StateNode, id| store
    .read_current(id)
    .ok_or_else(StateReadError::missing::<C>));
input!(
    Optional,
    Option<C::Snapshot>,
    |store: &C::StateNode, id| Ok(store.read_current(id))
);
input!(
    RequiredPair,
    SnapshotPair<C::Snapshot>,
    |store: &C::StateNode, id| store
        .read_values(id)
        .ok_or_else(StateReadError::missing::<C>)
);

impl<C: ExportConfig> StateInput for Required<C> {
    type GodotValue = Gd<DtoOf<C>>;
    fn godot_value(value: &Self::Values) -> Self::GodotValue {
        value.to_godot_owned()
    }
}
impl<C: ExportConfig> StateInput for Optional<C> {
    type GodotValue = Option<Gd<DtoOf<C>>>;
    fn godot_value(value: &Self::Values) -> Self::GodotValue {
        value.as_ref().map(ToGodot::to_godot_owned)
    }
}
impl<C: ExportConfig> StateInput for RequiredPair<C> {
    type GodotValue = Array<Gd<DtoOf<C>>>;
    fn godot_value(value: &Self::Values) -> Self::GodotValue {
        // Pair order is previous, current; mapped outputs can give these explicit field names.
        [
            value.previous.to_godot_owned(),
            value.current.to_godot_owned(),
        ]
        .into_iter()
        .collect()
    }
}

macro_rules! inputs_tuple {
    ($($ty:ident : $index:tt),+) => {
        impl<$($ty: StateInputs),+> StateInputs for ($($ty,)+) {
            type Stores = ($($ty::Stores,)+);
            type Values = ($($ty::Values,)+);
            fn resolve(host: &Gd<Node>, changes: &mut Vec<Gd<ExportChangeSource>>) -> Result<Self::Stores, StateReadError> {
                Ok(($($ty::resolve(host, changes)?,)+))
            }
            fn read(stores: &Self::Stores, id: i64) -> Result<Self::Values, StateReadError> {
                Ok(($($ty::read(&stores.$index, id)?,)+))
            }
        }
    };
}
inputs_tuple!(A:0);
inputs_tuple!(A:0,B:1);
inputs_tuple!(A:0,B:1,C:2);
inputs_tuple!(A:0,B:1,C:2,D:3);
inputs_tuple!(A:0,B:1,C:2,D:3,E:4);
inputs_tuple!(A:0,B:1,C:2,D:3,E:4,F:5);
inputs_tuple!(A:0,B:1,C:2,D:3,E:4,F:5,G:6);
inputs_tuple!(A:0,B:1,C:2,D:3,E:4,F:5,G:6,H:7);

pub type StateMappingFn<C> = fn(
    <<C as CombinedStateConfig>::Inputs as StateInputs>::Values,
) -> Option<<C as CombinedStateConfig>::Value>;
pub trait CombinedStateConfig: 'static {
    type Inputs: StateInputs;
    type Value: ExportValue + ToGodot;
    const VALUE_MAPPING_FN: StateMappingFn<Self>;
}

fn prepare<C: CombinedStateConfig>(
    stores: &<C::Inputs as StateInputs>::Stores,
    id: i64,
) -> Result<C::Value, StateReadError> {
    let values = C::Inputs::read(stores, id)?;
    (C::VALUE_MAPPING_FN)(values).ok_or(StateReadError {
        component: type_name::<C>(),
        reason: "value mapping returned no data for the declared inputs",
    })
}

/// One-time read and support checks use exactly the same rules as subscriptions.
pub fn read_combined_state<C: CombinedStateConfig>(
    host: &Gd<Node>,
    id: i64,
) -> Result<C::Value, StateReadError> {
    let stores = C::Inputs::resolve(host, &mut Vec::new())?;
    prepare::<C>(&stores, id)
}

struct SourceValue<C: CombinedStateConfig>(C::Value);
impl<C: CombinedStateConfig> Clone for SourceValue<C> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<C: CombinedStateConfig> ExportValue for SourceValue<C> {
    const NAME: &'static str = C::Value::NAME;
}
impl<C: CombinedStateConfig> UpdatePayload for SourceValue<C> {
    type Fields = <C::Value as UpdatePayload>::Fields;
    fn into_fields(self) -> Self::Fields {
        self.0.into_fields()
    }
    fn write_arguments(&self, arguments: &mut Array<Variant>) {
        self.0.write_arguments(arguments);
    }
}

pub fn subscribe_source<C: CombinedStateConfig>(
    host: Gd<Node>,
    entity: Gd<EntityMeta>,
    receiver: Gd<UpdateReceiver>,
) -> Option<Gd<UpdateSubscription>> {
    let id = entity.bind().entity_id;
    connect::<C>(host, id, Some(entity), receiver)
}
pub fn subscribe_source_id<C: CombinedStateConfig>(
    host: Gd<Node>,
    id: i64,
    receiver: Gd<UpdateReceiver>,
) -> Option<Gd<UpdateSubscription>> {
    connect::<C>(host, id, None, receiver)
}

fn connect<C: CombinedStateConfig>(
    host: Gd<Node>,
    id: i64,
    entity: Option<Gd<EntityMeta>>,
    receiver: Gd<UpdateReceiver>,
) -> Option<Gd<UpdateSubscription>> {
    // Validate the complete request before saving a connection or scheduling its first value.
    let mut changes = Vec::new();
    let stores = match C::Inputs::resolve(&host, &mut changes) {
        Ok(stores) => stores,
        Err(error) => {
            report_state_error(C::Value::NAME, id, &error);
            return None;
        }
    };
    if let Err(error) = prepare::<C>(&stores, id) {
        report_state_error(C::Value::NAME, id, &error);
        return None;
    }
    let mut delivery = UpdateDispatcher::resolve(&host)?;
    let batch = delivery.bind_mut().value_batch(move || {
        ExportValueBatch::new(&changes, move |id| match prepare::<C>(&stores, id) {
            Ok(value) => Some(SourceValue::<C>(value)),
            Err(error) => {
                report_state_error(C::Value::NAME, id, &error);
                None
            }
        })
    });
    batch.subscribe_for(host, id, entity, receiver)
}
