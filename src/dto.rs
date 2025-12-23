use godot::obj::NewGd;
use godot::prelude::{Gd, GodotClass};

pub trait ExportMeta {
    type Dto: GodotClass;
    fn to_dto(&self) -> Gd<Self::Dto>;
}

pub trait DTO {
    type Component;
}

/// Convert/update a Godot DTO from a Bevy component.
///
/// Design:
/// - User implements only `update(...)` (write exported fields into the DTO).
/// - `dto_from(...)` has a default impl: allocate `new_gd()`, then call `update(...)`.
pub trait DtoFrom<C>: DTO {
    /// Update an existing DTO in-place from the component.
    fn update(dto: &mut Gd<Self>, c: &C)
    where
        Self: Sized + GodotClass + NewGd;

    /// Allocate a new DTO and populate it by calling `update(...)`.
    fn dto_from(c: &C) -> Gd<Self>
    where
        Self: Sized + GodotClass + NewGd,
    {
        let mut dto = Self::new_gd();
        Self::update(&mut dto, c);
        dto
    }
}
