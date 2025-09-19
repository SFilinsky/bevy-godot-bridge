use godot::prelude::{Gd, GodotClass};

pub trait ExportMeta {
    type Dto: GodotClass;
    fn to_dto(&self) -> Gd<Self::Dto>;
}