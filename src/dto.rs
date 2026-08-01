use crate::import::subsystems::IdentitySubsystem;
use godot::obj::NewGd;
use godot::prelude::{Gd, GodotClass, Node, RefCounted};

/// Links a Rust data type (typically Message / Intention) with its Godot DTO,
/// and provides bidirectional conversion/update APIs.
///
/// - Store DTOs in NonSend queues (Godot-friendly, not Send)
/// - Drain queue in Bevy and convert DTO -> Rust data
pub trait DataTransferConfig {
    type DataType: Default + Sized;
    type DtoType: GodotClass + NewGd;

    /// Update an existing DTO in-place from Rust data.
    fn update_dto(
        dto: &mut Gd<Self::DtoType>,
        data: &Self::DataType,
        identity: &mut IdentitySubsystem,
    );

    /// Update an existing Rust data in-place from DTO.
    fn update_data(
        dto: &Gd<Self::DtoType>,
        data: &mut Self::DataType,
        identity: &mut IdentitySubsystem,
    );

    /// Convert DTO into Rust data.
    fn from_dto(dto: &Gd<Self::DtoType>, identity: &mut IdentitySubsystem) -> Self::DataType {
        let mut data = Self::DataType::default();
        Self::update_data(dto, &mut data, identity);
        data
    }

    /// Allocate a new DTO and populate it from Rust data.
    fn from_data(data: &Self::DataType, identity: &mut IdentitySubsystem) -> Gd<Self::DtoType> {
        let mut dto = Self::DtoType::new_gd();
        Self::update_dto(&mut dto, data, identity);
        dto
    }
}

/// Describes an export whose small changes can be merged before a visual-frame flush.
///
/// The bridge owns when pending changes are captured and sent. Implementations own only
/// the data-specific merge and DTO conversion rules, so gameplay does not depend on export timing.
pub trait IncrementalExportConfig: DataTransferConfig {
    type PendingChanges: Default + Send + Sync + 'static;

    /// Merges one changed source value into the bridge-owned pending buffer.
    /// A full snapshot replaces every older pending change.
    fn accumulate_pending_changes(
        pending_changes: &mut Self::PendingChanges,
        data: &Self::DataType,
    );

    /// Returns whether the pending buffer can initialize a consumer from scratch.
    fn pending_changes_are_full_snapshot(pending_changes: &Self::PendingChanges) -> bool;

    /// Writes the pending full snapshot or coalesced delta into an existing DTO.
    fn update_dto_from_pending_changes(
        dto: &mut Gd<Self::DtoType>,
        pending_changes: &Self::PendingChanges,
        identity: &mut IdentitySubsystem,
    );

    /// Returns whether the current source value can initialize a consumer without pending changes.
    fn data_is_full_snapshot(data: &Self::DataType) -> bool;
}

pub trait BuildsDto<Dto>
where
    Dto: GodotClass,
{
    fn build_dto(&self) -> Gd<Dto>;
}

pub trait WithGatherer: GodotClass<Base = RefCounted> {
    type Gatherer: GodotClass<Base = Node> + BuildsDto<Self>;
}

pub trait WithStateNode: GodotClass {
    type StateNode: GodotClass<Base = Node>;
}
