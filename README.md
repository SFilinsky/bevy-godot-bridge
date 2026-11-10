# Bevy Godot Bridge

This library lets a Godot scene run Bevy simulation, interact with it, and get updates reactively.

It is experimental and actively developed.

The library is published so other people can learn from the idea and follow the direction, not because the API is
stable. Backwards compatibility is not a goal yet. If a cleaner architecture requires changing public API, generated
nodes, or scene contracts, the cleaner architecture wins.

It was forked from [bevy_godot4](https://github.com/jrockett6/bevy_godot4), but the concept has changed substantially.

## Documentation

Bridge contracts should be documented in the bridge itself: code comments, module docs, this README, or docs under
`bevy-godot-docs/`. Public macros, traits, and Godot nodes should explain what they do and when to use them.

### Startup Initialization Contract

`BevyApp` owns the Bevy update loop. It waits for Godot scene initialization before the first Bevy update.

`InitializationCoordinator` owns the Godot-side startup pass:

1. Initializer nodes register themselves from their own `_ready()` callbacks.
2. The coordinator groups registered nodes by initialization phase.
3. `PreImport` initializers create any initializer nodes that must exist first.
4. Configuration initializers run next. This includes action static data and other settings-like imports.
5. Entity initializers run after that. This includes level-authored entity imports and their component imports.
6. Within each phase, initializers run in scene-tree order.
7. Initializers send startup data to `BevyApp`.
8. The coordinator marks `BevyApp` as scene-initialized.
9. On the next `BevyApp.process()`, queued imports are read and the first Bevy update runs.

This guarantees scene-authored imports are sent before gameplay systems can observe the Bevy world. The coordinator
must not call the Bevy update loop directly because initializer methods may need to find and change `BevyApp` while
submitting their data.

If a startup node affects how entities are spawned, it should use the configuration phase. If a startup node creates or
configures scene-authored entities, it should use the entity phase.

Scenes that do not include an `InitializationCoordinator` keep standalone behavior: `BevyApp` starts after its own
setup finishes, and any registered initializer node calls `initialize()` immediately with a one-time warning. Add a
coordinator when the scene has Godot-authored startup data that must be ordered before the first Bevy update or grouped
across multiple initialization phases.

### App profiles

An app profile is a typed Godot node that chooses one fixed Bevy setup. Select the profile node in the Godot editor;
there are no profile strings or runtime switches.

The profile owns only setup. Its `BevyApp` child remains the one app node that every other bridge node finds through
the existing `BevyApp::resolve()` path. Godot scripts do not need to know which profile is active.

Define each setup in its own Rust file:

```rust
use bevy::prelude::App;
use bevy_godot4::prelude::bevy_app_profiles;

pub fn configure_gameplay(app: &mut App) {
    app.add_plugins(GameplayPlugin);
}
```

List profiles once in their parent module. This macro creates the one Godot
entry point required by the library.

```rust
bevy_app_profiles! {
    GameplayBevyApp => gameplay::configure_gameplay,
    BenchmarkBevyApp => benchmark::configure,
}
```

Then author the profile and its app in a scene. The typed `bevy_app` property is visible in the Inspector.

```text
GameplayBevyApp
в””в”Ђв”Ђ BevyApp
```


## The Concept

The original idea was that Bevy could manipulate Godot scenes and drive the whole game. Rust's multithreaded resource
rules made that difficult: Godot objects had to be passed around as `NonSend` Bevy resources. It also meant the Godot
Editor lost much of its value when Bevy created everything at runtime.

The new concept focuses on what Bevy and Godot each do well.

### Bevy breakdown

Advantages:

- modular gameplay architecture
- a large ecosystem of plugins and tools that Rust code can use
- potential for better performance than GDScript, depending on the implementation

Disadvantages:

- weak UI and rendering tools
- without an editor and artist workflow, it is difficult for non-programmers to use

### Godot breakdown

Advantages:

- an editor
- flexible scene node system
- visually editable UI
- input handling
- simple cross-platform builds
- easy entry level for non-programmers

Disadvantages:

- it lacks enough structure for large gameplay systems, so projects need custom tooling and architecture work
- GDScript has limits that normal programming languages do not

### Getting good of both worlds

They complement each other, so the bridge combines them while keeping their responsibilities separate.

Godot treats Bevy as an isolated simulation and binds to its exported state to update the scene. Godot can also read
input and send it to Bevy through clearly defined actions.

Bevy gameplay logic should not know about Godot directly. Export is configured in a separate module, so the game stays
separable from Godot.

Godot serves as the front end and host for Bevy. Bevy serves as the simulation. If Bevy is removed from a project, the
underlying simulation is gone. If Godot is removed, Bevy can keep working with a different solution for presentation
and input.

### Long-term future

If Bevy develops a solid editor that allows non-programmers to work with the engine, it could make this bridge obsolete.
With a native solution, the workflow would be more straightforward.

### Finding a scene service

Look up a Bevy app from the host node that needs it. The nearest scene root owns the lookup. Apps, coordinators and generated state stores register with Godot, which updates their group membership on tree entry and exit. A state store must be a direct child of the resolved app. Missing or duplicate providers keep their existing diagnostics.

While a scene is entering, later siblings may already be parented without appearing in a live group. The root takes a temporary snapshot of its authored app and coordinator candidates. Detached scenes and partially mounted branches use scoped discovery. Callers can keep the resolved reference for their own lifetime. Rebind when the provider or owning scope changes.

### Following entity export costs

Enable `bridge-transport-profiling` only in a measured development/profiled
build, then set `EQUILIBRIA_ENTITY_PROFILE=1` for entity stage timings. The
exporter records exclusive totals for finding changes, saved-value updates,
Godot notification and cleanup. Nested child times are charged once. Normal
builds compile the fine timing implementation out. Compare with probes disabled
because recording itself has measurable cost. A zero creation/removal row means
that workload did not exercise it. Entity scene listeners run in a separate
Godot callback after Bevy returns. Include `Godot export: deliver changed features`
in export totals; later interpolation callbacks still need separate measurement.

### Receive only the features a visual uses

Generated state stores expose `changes()`. Pass these sources to
`EntityMeta.watch_changes(callback, sources)` and keep the returned subscription
ID. The callback receives the entity revision only when at least one named
source changed for that entity. An empty source list receives every entity
revision. Call `unwatch_changes(id)` when the consuming view leaves the tree.
Two consumers have independent subscriptions, even when they use the same source.

Author one `ExportDelivery` child under `BevyApp` and assign its `export_delivery` property. Generated spawners queue changed
entities, then this child invokes visual listeners at the next process priority
in the same frame. Do not independently pause this child or manually run another
Bevy update before its delivery: saved current/previous values belong to the
completed update and are reused by the next one. The phase is sequential on
Godot's main thread; it does not move scene access onto workers.

Scene creation and removal notifications retain their existing order. Creation
attaches the authored scene before queuing its initial update. Despawn cancels
queued updates before starting custom visual cleanup. Optional-feature removal
also counts as a feature change, even though `get_curr()` now returns null.
Existing broad `on_change` listeners still run, after filtered listeners. Events
and Messages keep their ordered queues and are not collapsed into state updates.

In measured builds, Godot's custom monitor `Bridge/<app ID>/Godot delivery ms`
shows the separate phase. Queue delay counters count frames and microseconds
until that phase starts; they do not include interpolation or GPU display delay.
