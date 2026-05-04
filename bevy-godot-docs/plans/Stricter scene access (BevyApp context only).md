
For better layer separation, we should make scene access stricter on Bevy level. 

Instead of SceneTreeRef which grants access to the whole Engine scene, we should give access to BevyAppSingleton, or it's subscene. 

The idea is that Bevy needs scene access to expose it's API, but they should be:
1. Bound to specific BevyApp instance, allowing multiple parallel Bevy instances in future
2. Restricted to access other Godot nodes. There should be really no reason to do it as communication methods can be exposed for Godot to provide all needed information. In the same way signals can be exposed for Godot to react to events on Bevy side. Godot is responsible for scene management, Bevy is responsible for simulating and communicating state.