//! Helpers for Godot scenes used by the bridge.

pub mod root_scripts;
pub mod scene_root;

use std::str::FromStr;

use crate::prelude::*;
use bevy::{
    app::{App, Plugin, PostUpdate},
    log::tracing,
    prelude::{Commands, Component, Entity, Query, Without},
};
use godot::obj::Singleton;
use godot::{
    builtin::{GString, Transform2D, Transform3D, Vector2, Vector3},
    classes::{Node2D, Node3D, PackedScene, ResourceLoader},
};

pub(crate) struct PackedScenePlugin;
impl Plugin for PackedScenePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(PostUpdate, spawn_scene);
    }
}

/// Tells the bridge to create a Godot packed scene.
///
/// The bridge adds the scene below this `BevyApp`'s Godot node parent. It also
/// keeps a reference so Bevy can find the created node later. Use the Godot
/// editor for normal game scene composition.
#[derive(Debug, Component)]
pub struct GodotScene {
    resource: GodotSceneResource,
    transform: Option<GodotSceneTransform>,
}

#[derive(Debug)]
enum GodotSceneResource {
    Resource(ErasedGdResource),
    Path(String),
    #[cfg(feature = "assets")]
    Handle(Handle<ErasedGdResource>),
}

#[derive(Debug)]
enum GodotSceneTransform {
    Transform2D(Transform2D),
    Transform3D(Transform3D),
}

impl GodotScene {
    /// Creates a request from a Godot packed scene that is already loaded.
    pub fn from_resource(res: ErasedGdResource) -> Self {
        Self {
            resource: GodotSceneResource::Resource(res),
            transform: None,
        }
    }

    /// Creates a request from a Godot resource path.
    ///
    /// This calls `ResourceLoader.load()` and may pause the game briefly. For a
    /// scene already loaded by Godot, use [`Self::from_resource`].
    pub fn from_path(path: &str) -> Self {
        Self {
            resource: GodotSceneResource::Path(path.to_string()),
            transform: None,
        }
    }

    /// Creates a spawn request from a Bevy asset handle.
    #[cfg(feature = "assets")]
    pub fn from_handle(handle: &Handle<ErasedGdResource>) -> Self {
        Self {
            resource: GodotSceneResource::Handle(handle.clone()),
            transform: None,
        }
    }

    /// Applies a 3D transform after the scene instance is attached to Godot.
    pub fn with_transform3d(mut self, transform: Transform3D) -> Self {
        self.transform = Some(GodotSceneTransform::Transform3D(transform));
        self
    }

    /// Applies a 2D transform after the scene instance is attached to Godot.
    pub fn with_transform2d(mut self, transform: Transform2D) -> Self {
        self.transform = Some(GodotSceneTransform::Transform2D(transform));
        self
    }

    /// Applies a 3D translation after the scene instance is attached to Godot.
    pub fn with_translation3d(mut self, translation: Vector3) -> Self {
        self.transform = Some(GodotSceneTransform::Transform3D(
            Transform3D::IDENTITY.translated(translation),
        ));
        self
    }

    /// Applies a 2D translation after the scene instance is attached to Godot.
    pub fn with_translation2d(mut self, translation: Vector2) -> Self {
        self.transform = Some(GodotSceneTransform::Transform2D(
            Transform2D::IDENTITY.translated(translation),
        ));
        self
    }
}

#[derive(Component, Debug, Default)]
struct GodotSceneSpawned;

fn spawn_scene(
    mut commands: Commands,
    mut new_scenes: Query<(&mut GodotScene, Entity), Without<GodotSceneSpawned>>,
    #[cfg(feature = "assets")] mut assets: ResMut<Assets<ErasedGdResource>>,
    app: BevyAppSubsystem,
) {
    let mut app_host = app.node_host();

    for (mut scene, ent) in new_scenes.iter_mut() {
        let packed_scene = match &mut scene.resource {
            GodotSceneResource::Resource(res) => res.get(),
            GodotSceneResource::Path(path) => ResourceLoader::singleton()
                .load(&GString::from_str(path).expect("path to be a valid GString"))
                .expect("packed scene to load"),
            #[cfg(feature = "assets")]
            GodotSceneResource::Handle(handle) => assets
                .get_mut(&handle)
                .expect("packed scene to exist in assets")
                .get(),
        };

        let instance = packed_scene
            .try_cast::<PackedScene>()
            .expect("resource to be a packed scene")
            .instantiate()
            .unwrap();

        app_host.add_child(&instance);

        if let Some(transform) = &scene.transform {
            match transform {
                GodotSceneTransform::Transform2D(transform) => {
                    match instance.clone().try_cast::<Node2D>().ok() {
                        Some(mut node2d) => node2d.set_global_transform(*transform),
                        None => tracing::error!(
                            "attempted to spawn a scene with a transform on Node that did not inherit from Node2D, the transform was not set"
                        ),
                    }
                }
                GodotSceneTransform::Transform3D(transform) => {
                    match instance.clone().try_cast::<Node3D>().ok() {
                        Some(mut node3d) => node3d.set_global_transform(*transform),
                        None => tracing::error!(
                            "attempted to spawn a scene with a transform on Node that did not inherit from Node3D, the transform was not set"
                        ),
                    }
                }
            }
        }

        commands
            .entity(ent)
            .insert(ErasedGd::new(instance))
            .insert(GodotSceneSpawned);
    }
}
