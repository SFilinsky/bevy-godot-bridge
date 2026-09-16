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
