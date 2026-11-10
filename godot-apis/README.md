# Godot APIs

Shared interfaces let independent Godot components written in Rust work together
without depending on each other's implementation. This crate depends only on
Godot bindings, not Bevy or a game's features.

Each module describes one way for components to communicate:

- [Updates](src/updates/README.md): connect a value source to a Rust function or
  a Godot callback.
