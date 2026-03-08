pub mod resources {
    use bevy::platform::collections::HashMap;
    use bevy::prelude::Resource;
    use godot::builtin::{Color, Vector3};
    use godot::classes::{ArrayMesh, MeshInstance3D, Node, StandardMaterial3D};
    use godot::obj::Gd;

    #[derive(Clone, Copy)]
    pub struct PathConfig {
        pub always_on_top: bool,
        pub world_origin: Option<Vector3>,
        pub y_offset: f32,
    }

    impl Default for PathConfig {
        fn default() -> Self {
            Self {
                always_on_top: false,
                world_origin: None,
                y_offset: 0.0,
            }
        }
    }

    /// A polyline in world space.
    #[derive(Clone)]
    pub struct PathRequest {
        pub points: Vec<Vector3>, // world points
        pub color: Color,
        pub config: PathConfig,
    }

    #[derive(Resource, Default)]
    pub struct DebugPathRequests {
        pub pending: HashMap<String, PathRequest>,
    }

    impl DebugPathRequests {
        pub fn set_path(
            &mut self,
            key: impl Into<String>,
            points: Vec<Vector3>,
            color: Color,
            config: PathConfig,
        ) {
            self.pending.insert(
                key.into(),
                PathRequest {
                    points,
                    color,
                    config,
                },
            );
        }

        /// Convenience: keep old behavior (single segment).
        pub fn set_line(
            &mut self,
            key: impl Into<String>,
            a: Vector3,
            b: Vector3,
            color: Color,
            config: PathConfig,
        ) {
            self.set_path(key, vec![a, b], color, config);
        }

        pub fn clear(&mut self, key: &str) {
            self.pending.remove(key);
        }

        pub fn clear_all(&mut self) {
            self.pending.clear();
        }
    }

    pub struct PathNode {
        pub mesh_instance: Gd<MeshInstance3D>,
        pub mesh: Gd<ArrayMesh>,
        pub material: Gd<StandardMaterial3D>,
    }

    #[derive(Default)]
    pub struct PathDriver {
        pub(super) parent: Option<Gd<Node>>,
        pub(super) nodes: HashMap<String, PathNode>,
    }
}

pub mod subsystem {
    use super::resources::DebugPathRequests;
    pub use super::resources::PathConfig;
    use bevy::ecs::system::SystemParam;
    use bevy::prelude::ResMut;
    use godot::builtin::{Color, Vector3};

    #[derive(SystemParam)]
    pub struct DebugPathSubsystem<'w> {
        requests: ResMut<'w, DebugPathRequests>,
    }

    impl<'w> DebugPathSubsystem<'w> {
        pub fn set_path(
            &mut self,
            key: impl Into<String>,
            point_list: Vec<Vector3>,
            color: Color,
            config: PathConfig,
        ) {
            self.requests.set_path(key, point_list, color, config);
        }

        pub fn set_line(
            &mut self,
            key: impl Into<String>,
            a: Vector3,
            b: Vector3,
            color: Color,
            config: PathConfig,
        ) {
            self.requests.set_line(key, a, b, color, config);
        }

        pub fn clear(&mut self, key: &str) {
            self.requests.clear(key);
        }

        pub fn clear_all(&mut self) {
            self.requests.clear_all();
        }
    }
}

pub mod systems {
    use crate::debug::debug_manager::DebugRenderGate;
    use crate::debug::paths::resources::{DebugPathRequests, PathDriver, PathNode};
    use bevy::platform::collections::HashSet;
    use bevy::prelude::{NonSendMut, ResMut};
    use bevy_godot4::debug::debug_manager::EDebugState;
    use godot::builtin::{
        NodePath, PackedColorArray, PackedVector3Array, VarArray, Variant, Vector3,
    };
    use godot::classes::base_material_3d::{Flags, ShadingMode};
    use godot::classes::mesh::{ArrayType, PrimitiveType};
    use godot::classes::{ArrayMesh, Engine, MeshInstance3D, SceneTree, StandardMaterial3D};
    use godot::meta::ToGodot;
    use godot::obj::{EngineEnum, Gd, NewAlloc, NewGd, Singleton};

    pub(super) fn ensure_parent(mut driver: NonSendMut<PathDriver>) {
        if driver.parent.is_some() {
            return;
        }

        let engine = Engine::singleton();
        let Some(loop_obj) = engine.get_main_loop() else {
            return;
        };
        let tree = loop_obj.cast::<SceneTree>();
        let Some(mut root) = tree.get_root() else {
            return;
        };

        let path = NodePath::from("BevyDebug");
        let parent: Gd<_> = if let Some(n) = root.try_get_node_as::<godot::classes::Node>(&path) {
            n
        } else {
            let mut n = godot::classes::Node::new_alloc();
            n.set_name("BevyDebug");
            root.add_child(&n);
            n
        };

        driver.parent = Some(parent);
    }

    pub(super) fn apply_paths(
        mut gate: DebugRenderGate,
        mut reqs: ResMut<DebugPathRequests>,
        mut driver: NonSendMut<PathDriver>,
    ) {
        let status = gate.get_status(EDebugState::Navmesh, 1.0 / 32.0);
        if !status.should_rerender {
            return;
        }

        let Some(mut parent) = driver.parent.as_ref().cloned() else {
            return;
        };

        let pending = std::mem::take(&mut reqs.pending);
        let pending_keys: HashSet<String> = pending.keys().cloned().collect();

        let stale_keys: Vec<String> = driver
            .nodes
            .keys()
            .filter(|k| !pending_keys.contains(*k))
            .cloned()
            .collect();

        for key in stale_keys {
            if let Some(node) = driver.nodes.remove(&key) {
                let mut mesh_instance = node.mesh_instance;
                mesh_instance.queue_free();
            }
        }

        for (key, req) in pending {
            if !status.is_visible {
                if let Some(entry) = driver.nodes.get(&key) {
                    let mut mesh_instance = entry.mesh_instance.clone();
                    mesh_instance.set_visible(false);
                }
                continue;
            }

            let entry = if let Some(entry) = driver.nodes.get(&key) {
                PathNode {
                    mesh_instance: entry.mesh_instance.clone(),
                    mesh: entry.mesh.clone(),
                    material: entry.material.clone(),
                }
            } else {
                let mut mi = MeshInstance3D::new_alloc();
                mi.set_name(&key);
                parent.add_child(&mi);

                let mesh = ArrayMesh::new_gd();
                mi.set_mesh(&mesh);

                let mut mat = StandardMaterial3D::new_gd();
                mat.set_shading_mode(ShadingMode::UNSHADED);
                mat.set_flag(Flags::ALBEDO_FROM_VERTEX_COLOR, true);
                mi.set_material_override(&mat);

                let node = PathNode {
                    mesh_instance: mi.clone(),
                    mesh: mesh.clone(),
                    material: mat.clone(),
                };
                driver.nodes.insert(key.clone(), node);

                PathNode {
                    mesh_instance: mi,
                    mesh,
                    material: mat,
                }
            };

            let mut mi = entry.mesh_instance;
            mi.set_visible(true);

            // Build line segments from polyline points.
            // LINES expects vertex pairs: (p0,p1), (p1,p2), ...
            if req.points.len() < 2 {
                // Hide by clearing surfaces
                let mut mesh = entry.mesh;
                mesh.clear_surfaces();
                continue;
            }

            let mut verts = PackedVector3Array::new();
            let mut cols = PackedColorArray::new();

            let y = req.config.y_offset;

            for w in req.points.windows(2) {
                let a = w[0];
                let b = w[1];

                verts.push(Vector3::new(a.x, a.y + y, a.z));
                verts.push(Vector3::new(b.x, b.y + y, b.z));

                cols.push(req.color);
                cols.push(req.color);
            }

            let mut arrays = VarArray::new();
            arrays.resize(ArrayType::MAX.ord() as usize, &Variant::nil());
            arrays.set(ArrayType::VERTEX.ord() as usize, &verts.to_variant());
            arrays.set(ArrayType::COLOR.ord() as usize, &cols.to_variant());

            let mut mesh = entry.mesh;
            mesh.clear_surfaces();
            mesh.add_surface_from_arrays(PrimitiveType::LINES, &arrays);

            let mut mat = entry.material;
            mat.set_flag(Flags::DISABLE_DEPTH_TEST, req.config.always_on_top);
            mi.set_material_override(&mat);

            if let Some(origin) = req.config.world_origin {
                let mut t = mi.get_transform();
                t.origin = origin;
                mi.set_transform(t);
            }
        }
    }
}

pub mod plugin {
    use crate::debug::paths::resources::{DebugPathRequests, PathDriver};
    use crate::debug::paths::systems::{apply_paths, ensure_parent};
    use bevy::app::{App, FixedPreUpdate, FixedUpdate, Plugin};

    pub struct DebugPathVisualizationPlugin;

    impl Plugin for DebugPathVisualizationPlugin {
        fn build(&self, app: &mut App) {
            app.init_resource::<DebugPathRequests>()
                .insert_non_send_resource(PathDriver::default())
                .add_systems(FixedPreUpdate, ensure_parent)
                .add_systems(FixedUpdate, apply_paths);
        }
    }
}
