use bevy_godot4::prelude::{StatePublication, bridge_dto};

#[bridge_dto(TestStateDto)]
#[derive(Debug, PartialEq)]
struct TestStateSnapshot {
    #[var]
    number: i32,
}

#[test]
fn pending_changes_are_invisible_and_publish_only_the_latest_value() {
    let mut state = StatePublication::default();
    let mut changes = Vec::new();
    assert!(state.enqueue(1, TestStateSnapshot { number: 10 }, 1));
    assert!(state.enqueue(1, TestStateSnapshot { number: 20 }, 2));
    assert!(state.current(1).is_none());
    assert!(state.queued_contains(1));
    state.publish(&mut changes);
    assert_eq!(changes, vec![(1, 2, false)]);
    let pair = state.pair(1).unwrap();
    assert_eq!(pair.current.number, 20);
    assert_eq!(pair.previous.number, 20);

    changes.clear();
    assert!(!state.enqueue(1, TestStateSnapshot { number: 20 }, 3));
    state.publish(&mut changes);
    assert!(changes.is_empty());
    assert!(state.enqueue(1, TestStateSnapshot { number: 30 }, 4));
    assert_eq!(state.current(1).unwrap().number, 20);
    state.publish(&mut changes);
    assert_eq!(state.pair(1).unwrap().previous.number, 20);
    assert_eq!(state.current(1).unwrap().number, 30);
}

#[test]
fn removal_repairs_dense_indices_and_readdition_starts_new_history() {
    let mut state = StatePublication::default();
    let mut changes = Vec::new();
    for id in 1..=3 {
        state.enqueue(id, TestStateSnapshot { number: id as i32 }, 1);
    }
    state.publish(&mut changes);
    changes.clear();
    state.remove(2, 2, false);
    assert!(state.contains(2));
    assert!(!state.queued_contains(2));
    state.publish(&mut changes);
    assert_eq!(changes, vec![(2, 2, false)]);
    assert!(state.current(2).is_none());
    assert_eq!(state.current(3).unwrap().number, 3);

    state.enqueue(2, TestStateSnapshot { number: 40 }, 3);
    state.publish(&mut changes);
    assert_eq!(state.pair(2).unwrap().previous.number, 40);
    state.remove(2, 4, true);
    state.publish(&mut changes);
    assert_eq!(changes.last(), Some(&(2, 4, true)));
}

#[test]
fn destroyed_unpublished_values_do_not_leave_saved_state() {
    let mut state = StatePublication::default();
    let mut changes = Vec::new();
    state.enqueue(1, TestStateSnapshot { number: 10 }, 1);
    state.remove(1, 2, true);
    state.publish(&mut changes);
    assert!(state.current(1).is_none());
    assert!(!state.queued_contains(1));
    assert_eq!(changes, vec![(1, 2, true)]);
}
