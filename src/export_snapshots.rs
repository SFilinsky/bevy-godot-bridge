//! Keep small exported values together so native readers avoid Godot object access.
//!
//! The generated state store publishes these copies only after its ordinary
//! equality check accepts a change. Godot objects remain available to script
//! readers; native snapshots are read-only copies of the same published state.

use std::collections::HashMap;

use godot::obj::bounds::DeclUser;
use godot::obj::{Bounds, Gd, GodotClass};

/// No extra copies: readers receive the existing Godot data object.
pub struct NoSnapshot;

/// How a transfer config keeps and reads its published Rust values.
/// Small `Copy` types with `From<&Dto>` use the compact snapshot store automatically.
pub trait SnapshotConfig<D: GodotClass>: 'static {
    type Value: Clone + 'static;
    type Store: Default;
    fn publish(store: &mut Self::Store, entity: i64, dto: &Gd<D>);
    fn remove(store: &mut Self::Store, entity: i64);
    fn read_current(
        store: &Self::Store,
        entity: i64,
        dto: impl FnOnce() -> Option<Gd<D>>,
    ) -> Option<Self::Value>;
    fn read(
        store: &Self::Store,
        entity: i64,
        dto_pair: impl FnOnce() -> Option<SnapshotPair<Gd<D>>>,
    ) -> Option<SnapshotPair<Self::Value>>;
}

impl<D: GodotClass> SnapshotConfig<D> for NoSnapshot {
    type Value = Gd<D>;
    type Store = ();
    fn publish(_: &mut (), _: i64, _: &Gd<D>) {}
    fn remove(_: &mut (), _: i64) {}
    fn read_current(_: &(), _: i64, dto: impl FnOnce() -> Option<Gd<D>>) -> Option<Gd<D>> {
        dto()
    }
    fn read(
        _: &(),
        _: i64,
        dto_pair: impl FnOnce() -> Option<SnapshotPair<Gd<D>>>,
    ) -> Option<SnapshotPair<Gd<D>>> {
        dto_pair()
    }
}

impl<D, S> SnapshotConfig<D> for S
where
    D: GodotClass + Bounds<Declarer = DeclUser>,
    S: Copy + for<'a> From<&'a D> + 'static,
{
    type Value = S;
    type Store = ExportSnapshots<S>;
    fn publish(store: &mut Self::Store, entity: i64, dto: &Gd<D>) {
        store.publish(entity, S::from(&*dto.bind()));
    }
    fn remove(store: &mut Self::Store, entity: i64) {
        store.remove(entity);
    }
    fn read_current(
        store: &Self::Store,
        entity: i64,
        _: impl FnOnce() -> Option<Gd<D>>,
    ) -> Option<S> {
        store.get(entity).map(|pair| pair.current)
    }
    fn read(
        store: &Self::Store,
        entity: i64,
        _: impl FnOnce() -> Option<SnapshotPair<Gd<D>>>,
    ) -> Option<SnapshotPair<S>> {
        store.get(entity)
    }
}

/// Current and previous published values; the initial pair contains the same value.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SnapshotPair<T> {
    pub current: T,
    pub previous: T,
}

/// One compact value array and an entity-to-slot lookup per exported feature.
pub struct ExportSnapshots<T> {
    index_map: HashMap<i64, usize>,
    value_list: Vec<(i64, SnapshotPair<T>)>,
}

impl<T> Default for ExportSnapshots<T> {
    fn default() -> Self {
        Self {
            index_map: HashMap::new(),
            value_list: Vec::new(),
        }
    }
}

impl<T: Copy> ExportSnapshots<T> {
    /// Publish an accepted change, after the owning store has checked equality.
    pub fn publish(&mut self, entity: i64, value: T) {
        if let Some(&index) = self.index_map.get(&entity) {
            let pair = &mut self.value_list[index].1;
            pair.previous = pair.current;
            pair.current = value;
        } else {
            self.index_map.insert(entity, self.value_list.len());
            self.value_list.push((
                entity,
                SnapshotPair {
                    current: value,
                    previous: value,
                },
            ));
        }
    }

    /// Return values by copy; callers cannot mutate the published state.
    pub fn get(&self, entity: i64) -> Option<SnapshotPair<T>> {
        self.index_map
            .get(&entity)
            .map(|&index| self.value_list[index].1)
    }

    /// Keep the remaining values compact and repair the moved entity's lookup.
    pub fn remove(&mut self, entity: i64) {
        if let Some(index) = self.index_map.remove(&entity) {
            self.value_list.swap_remove(index);
            if let Some((moved, _)) = self.value_list.get(index) {
                self.index_map.insert(*moved, index);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{ExportSnapshots, SnapshotPair};

    #[test]
    fn removal_repairs_moved_values_and_readdition_starts_fresh() {
        let mut values = ExportSnapshots::default();
        values.publish(10, 1);
        values.publish(20, 2);
        values.publish(30, 3);
        values.publish(20, 4);
        assert_eq!(
            values.get(20),
            Some(SnapshotPair {
                previous: 2,
                current: 4
            })
        );
        values.remove(10);
        assert_eq!(
            values.get(30),
            Some(SnapshotPair {
                previous: 3,
                current: 3
            })
        );
        values.remove(20);
        values.remove(20);
        assert_eq!(values.get(20), None);
        values.publish(20, 9);
        assert_eq!(
            values.get(20),
            Some(SnapshotPair {
                previous: 9,
                current: 9
            })
        );
        assert_eq!(values.value_list.len(), 2);
    }
}
