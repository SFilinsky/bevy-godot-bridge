//! Rules for changing data between Rust and Godot objects.
//!
//! A `DataTransferConfig` tells the bridge how to copy one Rust value into one
//! Godot object, and how to copy it back. Different macros use this same rule
//! for different jobs.

use crate::import::subsystems::IdentitySubsystem;
use godot::obj::NewGd;
use godot::prelude::{Gd, GodotClass, Node, RefCounted};

/// Describes how one Rust type and one Godot type copy data to each other.
///
/// A DTO is a small Godot object that carries data across the bridge. Methods
/// that write a DTO send data to Godot. Methods that write Rust data receive it
/// from Godot. A macro uses only the direction it needs.
pub trait DataTransferConfig {
    type DataType: Default + Sized;
    type DtoType: GodotClass + NewGd;

    /// Copies a Rust value into an existing Godot data object.
    fn update_dto(
        dto: &mut Gd<Self::DtoType>,
        data: &Self::DataType,
        identity: &mut IdentitySubsystem,
    );

    /// Copies a Godot data object into an existing Rust value.
    fn update_data(
        dto: &Gd<Self::DtoType>,
        data: &mut Self::DataType,
        identity: &mut IdentitySubsystem,
    );

    /// Makes a new Rust value from a Godot data object.
    fn from_dto(dto: &Gd<Self::DtoType>, identity: &mut IdentitySubsystem) -> Self::DataType {
        let mut data = Self::DataType::default();
        Self::update_data(dto, &mut data, identity);
        data
    }

    /// Makes a new Godot data object from a Rust value.
    fn from_data(data: &Self::DataType, identity: &mut IdentitySubsystem) -> Gd<Self::DtoType> {
        let mut dto = Self::DtoType::new_gd();
        Self::update_dto(&mut dto, data, identity);
        dto
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

/// Names the Godot node that stores this kind of entity data.
///
/// The node keeps the current and previous values. It also lets a Godot script
/// ask whether an entity has this data without using dynamic property names.
pub trait WithStateNode: GodotClass {
    type StateNode: GodotClass<Base = Node>;
}
