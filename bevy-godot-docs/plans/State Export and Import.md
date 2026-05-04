Introduce communication layer between Bevy and Godot which provides pattern for sharing data and events between engines.

## Data export

For data export, 4 things should be created: 
- Component itself - regular Bevy component, Rust struct
	- can't implement Godot classes as they are not thread-safe
- ComponentDTO (data transfer object) - Rust struct implementing Godot's `RefCounted`
	- should implement `From<Component>`
	- is constructed from component and sent to Godot, which can read it
- Exporter node - Rust struct implementing Godot's `Node`
	- is attached to BevyApp
	- exposes signals to reactively track changes - `created`, `updated`, `removed` 
	- signals share DTO with data to Godot
- Exporting system - Bevy system for pure exporting changes
	- tracks Bevy component state, for any changes creates DTO based on Component and triggers Exporter signals using it

## Data import

For data import, we create 2 entities:
- Importer node - Rust struct implementing Godot's `Node`
	- exposes arbitrary methods to send information from Godot side
	- can be state shared, but also events like player input
- 