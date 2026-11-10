# Reading several components together

A health bar needs current health and maximum health. Bevy exports those as
separate components. `combined_state!` declares how to read both and send their
numbers together. It does not decide how to draw the bar.

This folder runs while Rust **builds** the program: it generates classes and
methods. The shared code in [updates/sources.rs](../../../src/updates/sources.rs)
reads data and connects receivers while the game runs.

## Declare the inputs and output fields

```rust
use bevy_godot4::prelude::{Required, combined_state};

combined_state! {
    prefix: Health,
    inputs: {
        health: Required<HealthTransferConfig>,
        metadata: Required<HealthMetadataTransferConfig>,
    },
    outputs: { current: f32, maximum: f32 },
    value_mapping_fn: |health, metadata| Some(HealthValues {
        current: health.current,
        maximum: metadata.maximum,
    }),
}
```

Each config names a gameplay type and its snapshot. The snapshot declaration
already identifies its Godot DTO. See the [data guide](../../../src/data/README.md).

This creates `HealthValues` for Rust, `HealthCombinedDto` for one-time Godot reads,
`HealthCombinedStateConfig` for the input/mapping rules, and
`HealthCombinedStateNode` for Godot calls. The old public name was `HealthUpdateSource`.
The combined class has static methods; do not attach one instance per entity.
Individual `HealthStateNode` and metadata state nodes remain authored under BevyApp.

The mapping may be a named function or an inline closure. It receives all its
inputs as arguments and must not depend on changing outside state. Input order
sets mapping argument order; output order sets callback argument order.

## Read once, or subscribe

```gdscript
var health = HealthStateNode.resolve(self)
if health.implements(entity.entity_id):
    var component = health.get_current(entity.entity_id)
    print(component.current)

if HealthCombinedStateNode.implements(self, entity.entity_id):
    var values = HealthCombinedStateNode.get_current(self, entity.entity_id)
    print(values.current, values.maximum)

var subscription = HealthCombinedStateNode.subscribe(
    self, entity, UpdateReceiver.callback(_on_health_changed)
)

func _on_health_changed(current: float, maximum: float) -> void:
    health_bar.max_value = maximum
    health_bar.value = current
```

`implements` uses the same required-input and mapping rules as a combined read.
It does not register a connection or create DTOs. `get_current` returns the current
named result, without subscribing. Individual state nodes also expose `get_previous`.
Combining fields never removes access to an exported component on its own.

Keep the subscription until the view leaves the scene; cancel it on exit.
A level service can use `subscribe_id` with an entity ID instead of EntityMeta.
Rust consumers use these same methods with a native receiver. One shared
UpdateDispatcher per Bevy app sends updates to both receiver kinds.

## Choose which inputs may be absent

Omit outputs and the mapping to forward snapshots directly:

```rust
combined_state! {
    prefix: Animation,
    inputs: {
        attack: Optional<AttackTransferConfig>,
        movement: Optional<MovementTransferConfig>,
    },
}
```

Import `Optional` from the bridge prelude. A callback receives two DTOs; either
may be `null`. A one-time read has named `attack` and `movement` fields.

- `Required<C>` needs the component's current snapshot. A missing component
  rejects a subscription and logs its full Rust type and entity ID in development.
- `Optional<C>` allows that entity to lack the component. It supplies a snapshot
  or absence. A node with only optional inputs can support an entity with none
  of those features; that is an explicit choice by its author.
- `RequiredPair<C>` supplies previous and current snapshots. A forwarded one-time
  Godot read exposes them as a typed array in that order. A mapping can turn them
  into endpoints for animation instead.

All inputs still need their state nodes authored under BevyApp, even when optional.
Subscribe after initial state has been published, as entity creation callbacks do.
Required inputs do not mean "wait for this feature to appear later". Unexpected
missing data during delivery is also logged. Normal entity removal cancels its
connections before reads occur.

Individual state nodes use `subscribe`/`subscribe_id` for a present component.
If a feature is optional for the consumer, check `implements(id)` before
subscribing and explicitly handle absence, for example by hiding that section.
Clear saved values when changing the selected entity. An entity's set of features
is assumed to stay fixed; a missing feature does not leave a waiting subscription.

A mapping returning `None` means it cannot provide its declared result: `implements`
returns false, a read returns null, and subscription setup fails with a development
error. During delivery it skips that result and logs the failure. Describe truly
optional facts as optional fields, not as silently missing required inputs.

## Where the time goes

Changed components mark subscribed entity IDs. Repeated marks are combined so
one combined node reads an entity once per delivery. Its values are shared across
connections. Rust receivers get typed fields; script receivers use standard Godot
conversion. The argument list is created once for that delivery and reused for
its script receivers. No combined DTO is created just to call those receivers.

A one-time combined read does create its result DTO and any forwarded component
DTOs. Use subscriptions for ongoing changes rather than reading every frame.
No value is prepared for an entity with no active subscribers.

## Test-only inspection

Build the bridge with `bridge-test-api` only for Godot regression tests or
benchmarks that count entity updates. It adds `on_updated` and
`is_updated_in_revision`; neither is registered in ordinary builds.
Change trackers are a Rust implementation detail, not a Godot method.

## Files in this folder

- [declaration.rs](declaration.rs) reads the macro settings and rejects invalid ones.
- [generation.rs](generation.rs) creates concrete typed Godot classes. Godot does
  not support Rust's type parameters, so these concrete entry points are necessary.
- [mod.rs](mod.rs) connects declaration parsing to code generation.

An unusual case can supply an existing config with
`combined_state! { config: CustomConfig, name: CustomCombinedStateNode }`.
Its value type must implement standard `ToGodot` for the one-time read, as well
as the shared delivery traits. Most features should use the field declaration.
