use godot::prelude::{Gd, GodotClass};

pub trait ExportMeta {
    type Dto: GodotClass;
    fn to_dto(&self) -> Gd<Self::Dto>;
}

pub trait DTO {
    type Component;
}

pub trait DtoFrom<C> {
    fn dto_from(c: &C) -> Gd<Self>
    where
        Self: Sized, Self: GodotClass;
}