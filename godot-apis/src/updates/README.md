# Receiving updates

Use this API when a value source needs to call a consumer without knowing its
implementation. Both sides depend on these small shared definitions. For example,
a bridge can send progress values to an animation service without that service
depending on the bridge.

`UpdateReceiver` describes where values go. A native receiver accepts typed Rust
fields directly. A script receiver accepts a Godot `Callable`; the source converts
its values into Godot arguments only for that route. The source checks a native
receiver's field types once when connecting it.

This module does not store state, schedule updates or implement subscriptions.
The source's subscription system owns those jobs.

## Create a native receiver

```rust
use godot_apis::updates::UpdateReceiver;

// The receiving service decides what these values mean and how to apply them.
let receiver = UpdateReceiver::native(|(elapsed, interval): (f64, f64)| {
    println!("Progress: {elapsed} of {interval}");
});
```

For a GDScript method, create the alternative receiver in Godot:

```gdscript
var receiver = UpdateReceiver.callback(_on_progress_changed)

func _on_progress_changed(elapsed: float, interval: float) -> void:
    progress_bar.value = elapsed / interval
```

Both receivers can be passed to a source that supports this API. For example,
the Bevy bridge exposes `source.subscribe_id(owner, entity_id, receiver)` for entity
state. Keep its returned subscription for as long as updates are needed.

## Implement a source

Implement `UpdatePayload` for the value you publish. `Fields` and `into_fields`
describe the native arguments; `write_arguments` prepares the Godot arguments.
Call `receiver.bind().resolve::<YourPayload>()` when connecting, handle a type
mismatch before registering, then retain the returned `Receiver` for delivery.
The bridge's source macro generates the payload implementation for its users.
