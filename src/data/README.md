# Sharing captured values with Godot

Use this module to choose which gameplay facts cross the bridge. For example,
Bevy health may be 70; Godot needs that number to draw a health bar. The bridge
saves a copy first, so later gameplay changes cannot alter an update already queued.
That saved Rust value is called a **snapshot**.

GDScript cannot directly read an ordinary Rust struct. When it needs the whole
snapshot, the bridge creates a Godot object with the same fields. That object is
a **DTO**, short for data transfer object. A Rust receiver can use the snapshot
without creating that object.

## Declare the shared fields once

```rust
use bevy_godot4::prelude::bridge_dto;

#[bridge_dto(HealthDto)]
#[derive(PartialEq)]
pub struct HealthSnapshot {
    #[var]
    pub current: f32,
}
```

While building the program, this macro generates `HealthDto` and both directions
of conversion. It also records which DTO belongs to this snapshot. There is no
second handwritten field list or DTO type setting to keep in sync.
Every generated DTO extends `RefCounted`: Godot releases it when nobody keeps a
reference. The declaration accepts only the DTO name, not a choice of base class.

Godot-rust is the library that lets Rust call Godot. Its standard `ToGodot` and
`FromGodot` traits describe conversion to and from Godot values. The macro
implements those traits for our snapshots. Rust callers can use standard `.into()`
in both directions:

```rust
use godot::obj::Gd;

let snapshot = HealthSnapshot { current: 70.0 };
let dto: Gd<HealthDto> = snapshot.into();
let captured_again: HealthSnapshot = dto.into();
```

Passing the snapshot itself moves its fields into the DTO. To keep the original,
write `(&snapshot).into()` instead; this copies its fields. Similarly,
`(&dto).into()` captures a snapshot while keeping the Godot object reference.
Capturing a DTO always copies its fields because other code may still hold that
same Godot object. These conversions reuse the existing snapshot capture and
Godot conversion code. Queues retain the consuming `TransferSnapshot::into_dto`
operation; godot-rust's `ToGodot` and `FromGodot` remain available too.

## Connect gameplay data to those fields

`DataTransferConfig` has a different job: it chooses how to copy gameplay data
into a snapshot and how to apply an incoming snapshot to gameplay data. It names
only `DataType` and `Snapshot`, and implements `update_snapshot` and `update_data`.
It does not convert gameplay data directly into a DTO.

For example, a movement component can contain a long planned path. Its snapshot
can contain just the position and facing direction Godot needs. An entity
reference can become the numeric ID shared with Godot. These choices belong to
the game; a general conversion library cannot infer them.

Code that needs the DTO type can write `DtoOf<HealthTransferConfig>`. This looks
up the type already declared by `bridge_dto`; it adds no new choice.

A copied gameplay component could itself be a snapshot if its fields match what
should be shared. A separate type is useful when we omit internal fields or change
references into shared IDs. Fewer type names alone do not imply faster execution.

## Follow an outgoing update

1. The config copies selected gameplay fields into a Rust snapshot.
2. The outgoing queue holds it until the Godot delivery step publishes it.
3. An individual state node keeps the component readable. A combined state node
   can read several components and prepare one set of values for a consumer.
4. A Rust receiver gets Rust fields. A GDScript callback gets ordinary Godot
   arguments, converted with godot-rust. Numbers remain numbers. A forwarded
   snapshot becomes its generated DTO only at this step.

See [combined state declarations](../../proc_macros/src/combined_state/README.md).
An individual `get_current` read creates its DTO on demand and caches it for the
published value. A combined `get_current` creates its named result when requested.
Neither read subscribes the caller to future changes.

## Follow an incoming request

Godot authors a DTO. The import queue captures its fields immediately, then
Bevy later reads the saved snapshot through `DataTransferConfig`.

If Godot sends an amount of 10 and then edits the same DTO to 20, the first
request must still contain 10. Keeping a reference to the editable DTO would
break that rule. This is why capture happens when a request enters the queue.

## Optional fields and copying limits

Godot-rust already converts numbers, vectors, strings and object handles. We do
not keep a second list of supported callback types. A mapped `Option<f32>` field
is handled by generated `Some`/`None` matching: convert the number or send Godot
`null`. This uses Godot's general value container, `Variant`, because the pinned
library version supports nullable object types but not nullable numbers directly.

Snapshot capture duplicates recognized Godot array/dictionary fields and captures
fields marked `#[nested]` recursively. Copying an ordinary Godot object handle
still points to the same object. A snapshot does not freeze arbitrary objects.
Use captured fields or nested snapshots for mutable data that must stay independent.
Treat DTOs read from state as read-only data; changing them does not update Bevy.

## Find the code

- [config.rs](config.rs): gameplay-to-snapshot mapping. `BuildsDto` and
  `WithGatherer` also describe how scene nodes supply authored settings.
- [types.rs](types.rs): the snapshot/DTO relationship, consuming conversion and
  capture from an existing DTO. `bridge_dto` generates these implementations.
- [import queue](../import/queue.rs): captures values when Godot submits them.
