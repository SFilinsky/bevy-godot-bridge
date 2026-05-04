
Markers are needed to include entity into some category of things - archetype or status - like `Unit`, `Building`, `OnFire`, `Untargetable` and so on.

There are two ways to define markers in ECS, each with it's advantages and disadvantages:

- Structural - marker components like `Unit`, `Burning`, `Stunned`
- Featural - “feature” component holding flags/arrays/enums/names, like `Statuses` or `UnitType`
## Structural 

Zero-sized components, systems query them via `With<T>`, `Without<T>`, `Changed<T>`, `RemovedComponents<T>`.

**Pros**

- Super fast filtering (archetype-level). Systems only see matching entities.
- Fine-grained change detection per tag (add/remove = change).
- Clear, composable queries (`With<A>, Or<(With<B>, With<C>)>`).
- Plays perfectly with Bevy patterns (Bundles, events, scheduling).

**Cons**

- Every unique tag set = new archetype. Lots of highly dynamic tags → archetype churn.
- Toggling tags every frame moves entities between archetypes (costly if frequent/high volume).
- Export/serialize needs bundling multiple components.

**Use when**

- Tags change **infrequently** (e.g., type: `Unit`, `Projectile`, factions).
- Systems are **selective** (small subset of entities match).
- You want clean, declarative filters and built-in change/removed detection.

## Featural

Components that represent specific game feature hold metainformation: 

```rs
struct Unit {
	type: string
}

trait Status {
    ...
}

struct Statuses {
    statuses: Vec<Box<dyn Status>>
}
```

Systems then can query specific feature they need and process metadata.

**Pros**

- Stable archetypes (entities keep the same component set).
- Easy to serialize/export as a single DTO.
- Switching many flags at runtime is just a field write (no archetype move).
- Can handle generic markers, allowing scaling in data-driven way without coding

**Cons**

- Systems must iterate more and branch inside (`if features.has(…) { … }`).
- `Changed<Features>` fires for _any_ bit change—no per-flag change tracking unless you diff.
- No free `RemovedComponents<T>`; you implement your own edge detection/events.

**Use when**

- Flags change **often** (per-frame, in bursts).
- Most systems look at **many** entities anyway (low selectivity).
- You want centralized snapshotting (network/export).
- Data-driven scalability is required