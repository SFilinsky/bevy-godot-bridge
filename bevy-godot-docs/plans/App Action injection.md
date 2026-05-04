## The Idea

To allow Feature flag system, it would require to signal Bevy app somehow when feature is enabled or disabled. 

We can introduce App action queue which BevyAppSingleton will drain every time and allow running polymorphic actions on App, to add/remove feature flag resources or register new systems.

