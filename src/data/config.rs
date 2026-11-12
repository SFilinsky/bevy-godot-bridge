//! Maps gameplay facts to captured bridge values and back.
use crate::data::types::TransferSnapshot;
use crate::import::subsystems::IdentitySubsystem;
use godot::classes::{Node, RefCounted};
use godot::obj::{Gd, GodotClass};

/// Maps gameplay data to an owned bridge value and back.
/// Queues keep snapshots; Godot DTOs are created only at a script boundary.
pub trait DataTransferConfig {
    type DataType: Default + Sized;
    type Snapshot: TransferSnapshot;

    fn update_snapshot(
        value: &mut Self::Snapshot,
        data: &Self::DataType,
        identity: &mut IdentitySubsystem,
    );
    fn update_data(
        value: &Self::Snapshot,
        data: &mut Self::DataType,
        identity: &mut IdentitySubsystem,
    );

    fn snapshot_from_data(
        data: &Self::DataType,
        identity: &mut IdentitySubsystem,
    ) -> Self::Snapshot {
        let mut value = Self::Snapshot::default();
        Self::update_snapshot(&mut value, data, identity);
        value
    }

    fn from_snapshot(value: &Self::Snapshot, identity: &mut IdentitySubsystem) -> Self::DataType {
        let mut data = Self::DataType::default();
        Self::update_data(value, &mut data, identity);
        data
    }
}

/// Builds a Godot data object from a Godot node.
///
/// Import macros use this to read values that a designer set in the scene.
pub trait BuildsDto<Dto>
where
    Dto: GodotClass,
{
    fn build_dto(&self) -> Gd<Dto>;
}

/// Names the Godot node that reads scene data for this data object.
pub trait WithGatherer: GodotClass<Base = RefCounted> {
    type Gatherer: GodotClass<Base = Node> + BuildsDto<Self>;
}
