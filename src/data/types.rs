//! One owned value crosses the bridge; a typed Godot DTO is built only on demand.
use crate::data::config::DataTransferConfig;
use godot::classes::RefCounted;
use godot::meta::{ByValue, FromGodot, ToGodot};
use godot::obj::bounds::DeclUser;
use godot::obj::{Bounds, Gd, GodotClass, NewGd};

/// Find the Godot object type from a config without declaring that type twice.
pub type DtoOf<C> = <<C as DataTransferConfig>::Snapshot as TransferSnapshot>::Dto;

/// Implemented by `bridge_dto`; queues and native readers keep this Rust value.
pub trait TransferSnapshot:
    Clone + Default + ToGodot<Via = Gd<Self::Dto>, Pass = ByValue> + FromGodot + 'static
{
    type Dto: GodotClass + NewGd + Bounds<Declarer = DeclUser> + TransferDto<Snapshot = Self>;
    fn into_dto(self) -> Gd<Self::Dto>;
}

/// Godot can author a value or inspect a published copy without borrowing Bevy.
pub trait TransferDto: GodotClass<Base = RefCounted> {
    type Snapshot: TransferSnapshot<Dto = Self>;
    fn capture_snapshot(&self) -> Self::Snapshot;
}
