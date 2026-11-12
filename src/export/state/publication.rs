//! Queue owned entity values, then publish them together after Bevy returns.
use crate::data::types::TransferSnapshot;
use crate::export::state::snapshots::SnapshotPair;
use godot::obj::Gd;
use std::cell::OnceCell;
use std::collections::HashMap;
use std::mem::{replace, take};

struct Entry<T: TransferSnapshot> {
    pair: SnapshotPair<T>,
    revision: i64,
    current_dto: OnceCell<Gd<T::Dto>>,
    previous_dto: OnceCell<Gd<T::Dto>>,
}

struct Pending<T> {
    value: Option<T>,
    revision: i64,
    forget: bool,
}

/// One feature's queue and published state; no Godot DTO is required to publish.
pub struct StatePublication<T: TransferSnapshot> {
    index_map: HashMap<i64, usize>,
    entry_list: Vec<(i64, Entry<T>)>,
    pending_map: HashMap<i64, Pending<T>>,
    pending_order: Vec<i64>,
}

impl<T: TransferSnapshot> Default for StatePublication<T> {
    fn default() -> Self {
        Self {
            index_map: HashMap::new(),
            entry_list: Vec::new(),
            pending_map: HashMap::new(),
            pending_order: Vec::new(),
        }
    }
}

impl<T: TransferSnapshot> StatePublication<T> {
    pub fn enqueue(&mut self, id: i64, value: T, revision: i64) -> bool
    where
        T: PartialEq,
    {
        if id < 0 {
            return false;
        }
        // Compare with the last queued value so repeated source changes do no extra work.
        let previous = self
            .pending_map
            .get(&id)
            .and_then(|pending| pending.value.as_ref());
        let previous = if self.pending_map.contains_key(&id) {
            previous
        } else {
            self.entry(id).map(|entry| &entry.pair.current)
        };
        if previous == Some(&value) {
            return false;
        }
        self.queue(
            id,
            Pending {
                value: Some(value),
                revision,
                forget: false,
            },
        );
        true
    }

    pub fn remove(&mut self, id: i64, revision: i64, forget: bool) {
        self.queue(
            id,
            Pending {
                value: None,
                revision,
                forget,
            },
        );
    }

    fn queue(&mut self, id: i64, pending: Pending<T>) {
        if self.pending_map.insert(id, pending).is_none() {
            self.pending_order.push(id);
        }
    }

    /// Apply final queued values before notifying any readers or creating scenes.
    pub fn publish(&mut self, changed: &mut Vec<(i64, i64, bool)>) {
        for id in self.pending_order.drain(..) {
            let pending = self
                .pending_map
                .remove(&id)
                .expect("queued state has a pending value");
            if let Some(value) = pending.value {
                if let Some(&index) = self.index_map.get(&id) {
                    let entry = &mut self.entry_list[index].1;
                    entry.pair.previous = replace(&mut entry.pair.current, value);
                    entry.previous_dto = take(&mut entry.current_dto);
                    entry.revision = pending.revision;
                } else {
                    self.index_map.insert(id, self.entry_list.len());
                    self.entry_list.push((
                        id,
                        Entry {
                            pair: SnapshotPair {
                                previous: value.clone(),
                                current: value,
                            },
                            revision: pending.revision,
                            current_dto: OnceCell::new(),
                            previous_dto: OnceCell::new(),
                        },
                    ));
                }
            } else if let Some(index) = self.index_map.remove(&id) {
                self.entry_list.swap_remove(index);
                if let Some((moved, _)) = self.entry_list.get(index) {
                    self.index_map.insert(*moved, index);
                }
            }
            changed.push((id, pending.revision, pending.forget));
        }
    }

    fn entry(&self, id: i64) -> Option<&Entry<T>> {
        self.index_map
            .get(&id)
            .map(|&index| &self.entry_list[index].1)
    }
    pub fn contains(&self, id: i64) -> bool {
        self.index_map.contains_key(&id)
    }
    pub fn queued_contains(&self, id: i64) -> bool {
        self.pending_map
            .get(&id)
            .map_or_else(|| self.contains(id), |pending| pending.value.is_some())
    }
    pub fn current(&self, id: i64) -> Option<T> {
        self.entry(id).map(|entry| entry.pair.current.clone())
    }
    pub fn pair(&self, id: i64) -> Option<SnapshotPair<T>> {
        self.entry(id).map(|entry| entry.pair.clone())
    }
    pub fn current_dto(&self, id: i64) -> Option<Gd<T::Dto>> {
        self.entry(id).map(|entry| {
            entry
                .current_dto
                .get_or_init(|| entry.pair.current.clone().into_dto())
                .clone()
        })
    }
    pub fn previous_dto(&self, id: i64) -> Option<Gd<T::Dto>> {
        self.entry(id).map(|entry| {
            entry
                .previous_dto
                .get_or_init(|| entry.pair.previous.clone().into_dto())
                .clone()
        })
    }
}
