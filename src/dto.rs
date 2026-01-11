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
    fn update_dto(dto: &mut Gd<Self::DtoType>, data: &Self::DataType);

    /// Update an existing Rust data in-place from DTO.
    fn update_data(dto: &Gd<Self::DtoType>, data: &mut Self::DataType);

    /// Convert DTO into Rust data.
    fn from_dto(dto: &Gd<Self::DtoType>) -> Self::DataType {
        let mut data = Self::DataType::default();
        Self::update_data(dto, &mut data);
        data
    }

    /// Allocate a new DTO and populate it from Rust data.
    fn from_data(data: &Self::DataType) -> Gd<Self::DtoType> {
        let mut dto = Self::DtoType::new_gd();
        Self::update_dto(&mut dto, data);
        dto
    }
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
