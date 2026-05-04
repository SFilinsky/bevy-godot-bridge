# Bevy Godot Bridge

This library allows using Bevy simulation inside Godot scenes and binding to the Bevy state. 

At the moment of writing (May 2026) it's developed actively and backwards compatibility is not guaranteed.

It was forked from [bevy_godot4](https://github.com/jrockett6/bevy_godot4), but the concept changed dramatically.

## The Concept

Original repo idea was "what if Bevy could manipulate Godot scenes to drive the whole game". However, as Rust is 
designed for multithreaded resource safety, Godot caused many complications. Everything had to be passed around
as NonSend Bevy resources. Also, it meant Godot Editor loses a lot of its power as Bevy bootstrapped things in runtime.

The new concept focuses on using Bevy and Godot strong parts. 

### Bevy breakdown

Advantages:
- thought-out modular gameplay architecture
- large ecosystem of plugins and tools – anything from Rust can be integrated basically
- potential for higher performance than GDScript, depending on the implementation

Disadvantages:
- weak around UI and rendering
- with no editor and artist workflow, it's practically inaccessible for non-programmers

### Godot breakdown

Advantages:

- an editor
- flexible scene node system
- visually editable UI
- input handling
- simple cross-platform builds
- easy entry level for non-programmers

Disadvantages:
- it lacks structure – any meaningful gameplay would require a lot of custom tooling and architecture work
- GDScript is deficient in many ways and inferior to normal programming languages

### Getting good of both worlds

They compliment each other perfectly, so instead of fighting with Godot new approach tries to combine them in the 
way where they cleanly separate those responsibilities. 

Godot treats Bevy as "isolated black box" and can simply bind to its outputs (export) to manipulate the scene on 
its own. Also, Godot can read input and inject it into Bevy via cleanly defined actions.

Bevy gameplay logic has the premise of knowing nothing about Godot directly, all the export is configured as separate 
module, and as a litmus test the game stays separable from Godot. 

Godot basically serves as "front-end", or "wrapping", or "host" for Bevy. Bevy serves as "brain".
If the Bevy part is removed from a project, nothing is left as the underlying simulation is gone. However, if Godot is
 removed, Bev will keep working and can use a different solution for presentation and input.

### Long-term future

If Bevy ever develops to having a solid editor that allows non-programmers to work with the engine, it will make this 
bridge obsolete. With a native solution, the workflow will just become more straightforward.
