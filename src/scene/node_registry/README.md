# NodeRegistry

Use this bridge API when independently authored scenes share nodes. Wire local
scene dependencies in the Inspector or pass known references first. Do not
resolve nodes every frame.

The same nine method names are available in Rust and GDScript. Registration
derives the class from the provider. GDScript lookup takes a class reference,
not a group name or string. Rust lookup takes a type parameter.

| Method | Use |
| --- | --- |
| `register_level_instance(node)` | Several providers of this class in the nearest SceneRoot. |
| `register_level_singleton(node)` | One provider of this class in the nearest SceneRoot. |
| `register_owned_instance(node, owner)` | Several providers belonging to this exact owner. |
| `register_owned_singleton(node, owner)` | One provider belonging to this exact owner. |
| `find_level_singleton(host, node_class)` | One node or null, in the host's nearest SceneRoot. |
| `find_owned_singleton(owner, node_class)` | One node or null, under the explicit owner. |
| `find_level_instances(host, node_class)` | Snapshot array of the level's registered instances. |
| `find_owned_instances(owner, node_class)` | Snapshot array belonging to the explicit owner. |
| `unregister(node)` | Withdraw all registrations for this node now. |

```gdscript
func _enter_tree() -> void:
    NodeRegistry.register_level_singleton(self)

# Consumer, after its provider has entered the tree:
var manager := NodeRegistry.find_level_singleton(self, SoundManager) as SoundManager

# A separate example with multiple parts and an explicit building reference:
NodeRegistry.register_owned_instance(self, building_root)
var parts := NodeRegistry.find_owned_instances(building_root, BuildingPart)
```

```rust,ignore
NodeRegistry::register_owned_instance(&self.to_gd(), &building_root)?;
let parts = NodeRegistry::find_owned_instances::<BuildingPart>(&building_root)?;
```

Rust registration returns `Result<(), RegistryError>`. Lookup returns
`Result<Option<Gd<T>>, RegistryError>` or `Result<Vec<Gd<T>>, RegistryError>`.
GDScript registration returns bool. Invalid usage prints a developer error and
returns false, null or an empty array. Missing providers are normal empty results.

## Owners and lifetime

Level means the nearest SceneRoot, never the whole game. Nested levels remain
separate. Owned methods require an explicit node, which need not be an ancestor
of the provider. Both must be in the same SceneTree. An explicit viewport owner
works for a standalone preview without a SceneRoot.

Call registration in `_enter_tree`, after the provider has a parent and has
entered the tree. Registering the same instance again is harmless. A different
instance cannot claim an occupied singleton slot. Singleton and instance modes
cannot share the same class slot. Classes are inferred from the attached script
when present, otherwise from the native class.

The owner holds its index. It contains node IDs and does not keep providers alive.
Provider tree exit removes its registrations; owner tree exit clears its index.
Re-entering providers must register again. A provider elsewhere in the tree must
also register again if its explicit owner was removed and re-added. Manual
unregister is safe to repeat. Moving to a different owner means unregister then
register with that owner.

Registration means the provider exists; it does not mean `_ready` has completed.
A consumer's `_ready` can precede entry of a later sibling. Resolve after the
known provider enters, pass a reference, or defer consumer setup until the whole
level is ready. Registry lookup never searches for an unregistered provider.
The bridge's app/coordinator startup adapter separately handles early and
detached authored scenes. Do not copy that fallback into ordinary feature code.

Returned arrays are snapshots. Use existing feature signals when a collection
changes; this API does not introduce registry-wide change signals. Release
subscriptions and replace cached references when their providers change.

## Implementation boundaries

The registry is ordinary runtime infrastructure, not a measurement feature.
Observability added around it must use the bridge's existing dev/profiled guards.
Do not add gameplay names or implicit building-root discovery to this module.

GDScript script classes use their Script identity, including local classes without
`class_name`. Godot does not expose native class-reference names to extensions, so
native GDScript lookups use a tiny internal `is_instance_of` helper over registered
candidates only. It never constructs a dummy provider or walks scene descendants.
