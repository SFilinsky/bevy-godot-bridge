pub mod resources {
    use bevy::math::Vec3;
    use bevy::platform::collections::HashMap;
    use bevy::prelude::{Resource, Transform};
    use godot::builtin::Color;
    use godot::classes::{BoxMesh, MeshInstance3D, Node, StandardMaterial3D};
    use godot::obj::Gd;

    #[derive(Clone, Copy, Debug)]
    pub struct CuboidConfig {
        pub always_on_top: bool,
        pub wireframe: bool,
        pub unshaded: bool,
        pub transparent: bool,
    }
    impl Default for CuboidConfig {
        fn default() -> Self {
            Self {
                always_on_top: false,
                wireframe: false,
                unshaded: true,
                transparent: true,
            }
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub struct CuboidRequest {
        pub transform: Transform,
        pub half_extents: Vec3,
        pub color: Color,
        pub config: CuboidConfig,
    }

    #[derive(Resource, Default)]
    pub struct DebugCuboidRequests {
        pub pending: HashMap<String, CuboidRequest>,
        pub to_clear: Vec<String>,
        pub clear_all: bool,
    }

    impl DebugCuboidRequests {
        pub fn set_cuboid(
            &mut self,
            key: impl Into<String>,
            transform: Transform,
            half_extents: Vec3,
            color: Color,
            config: CuboidConfig,
        ) {
            self.pending.insert(
                key.into(),
                CuboidRequest {
                    transform,
                    half_extents,
                    color,
                    config,
                },
            );
        }

        pub fn clear_cuboid(&mut self, key: impl Into<String>) {
            self.to_clear.push(key.into());
        }

        pub fn clear_all(&mut self) {
            self.clear_all = true;
        }
    }

    pub struct CuboidNode {
        pub mesh_instance: Gd<MeshInstance3D>,
        pub mesh: Gd<BoxMesh>,
        pub material: Gd<StandardMaterial3D>,
    }

    #[derive(Default)]
    pub struct CuboidDriver {
        pub(super) parent: Option<Gd<Node>>,
        pub(super) nodes: HashMap<String, CuboidNode>,
    }
}

pub mod systems {
    use super::resources::{CuboidDriver, CuboidNode, DebugCuboidRequests};
    use crate::debug::debug_manager::DebugRenderGate;
    use bevy::prelude::{NonSendMut, ResMut, Transform};
    use bevy_godot4::prelude::EDebugState;
    use godot::builtin::{Basis, NodePath, Quaternion, Transform3D, Vector3};
    use godot::classes::base_material_3d::{CullMode, Flags, ShadingMode, Transparency};
    use godot::classes::{BoxMesh, Engine, MeshInstance3D, SceneTree, StandardMaterial3D};
    use godot::obj::{Gd, NewAlloc, NewGd, Singleton};

    pub fn bevy_to_godot_transform(t: &Transform) -> Transform3D {
        let rot = Quaternion::new(t.rotation.x, t.rotation.y, t.rotation.z, t.rotation.w);
        let basis = Basis::from_quaternion(rot);
        let origin = Vector3::new(t.translation.x, t.translation.y, t.translation.z);
        Transform3D::new(basis, origin)
    }

    pub(super) fn ensure_parent(mut driver: NonSendMut<CuboidDriver>) {
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

    pub(super) fn apply_cuboids(
        mut gate: DebugRenderGate,
        mut reqs: ResMut<DebugCuboidRequests>,
        mut driver: NonSendMut<CuboidDriver>,
    ) {
        let status = gate.get_status(EDebugState::Colliders, 0.5);

        if !status.should_rerender {
            return;
        }

        let Some(mut parent) = driver.parent.as_ref().cloned() else {
            return;
        };

        if reqs.clear_all {
            for (_, n) in driver.nodes.drain() {
                let mut mesh_instance = n.mesh_instance;
                mesh_instance.queue_free();
            }
            reqs.clear_all = false;
        } else {
            for key in reqs.to_clear.drain(..) {
                if let Some(n) = driver.nodes.remove(&key) {
                    let mut mesh_instance = n.mesh_instance;
                    mesh_instance.queue_free();
                }
            }
        }

        if reqs.pending.is_empty() {
            return;
        }

        let pending = std::mem::take(&mut reqs.pending);

        for (key, req) in pending {
            if !status.is_visible {
                if let Some(entry) = driver.nodes.get(&key) {
                    let mut mesh_instance = entry.mesh_instance.clone();
                    mesh_instance.set_visible(false);
                }
                continue;
            }

            let entry = if let Some(entry) = driver.nodes.get(&key) {
                CuboidNode {
                    mesh_instance: entry.mesh_instance.clone(),
                    mesh: entry.mesh.clone(),
                    material: entry.material.clone(),
                }
            } else {
                let mut mi = MeshInstance3D::new_alloc();
                mi.set_name(&key);
                parent.add_child(&mi);

                let mesh = BoxMesh::new_gd();
                mi.set_mesh(&mesh);

                let mat = StandardMaterial3D::new_gd();
                mi.set_material_override(&mat);

                let node = CuboidNode {
                    mesh_instance: mi.clone(),
                    mesh: mesh.clone(),
                    material: mat.clone(),
                };
                driver.nodes.insert(key.clone(), node);

                CuboidNode {
                    mesh_instance: mi,
                    mesh,
                    material: mat,
                }
            };

            let mut mi = entry.mesh_instance;
            mi.set_visible(true);

            let size = Vector3::new(
                req.half_extents.x * 2.0,
                req.half_extents.y * 2.0,
                req.half_extents.z * 2.0,
            );

            let mut mesh = entry.mesh;
            mesh.set_size(size);

            let mut mat = entry.material;

            if req.config.unshaded {
                mat.set_shading_mode(ShadingMode::UNSHADED);
            } else {
                mat.set_shading_mode(ShadingMode::PER_PIXEL);
            }

            if req.config.transparent || (req.color.a < 0.999) {
                mat.set_transparency(Transparency::ALPHA);
            } else {
                mat.set_transparency(Transparency::DISABLED);
            }

            mat.set_flag(Flags::DISABLE_DEPTH_TEST, req.config.always_on_top);
            mat.set_albedo(req.color);
            mat.set_flag(Flags::ALBEDO_FROM_VERTEX_COLOR, false);
            mat.set_cull_mode(CullMode::DISABLED);

            mi.set_material_override(&mat);

            mi.set_transform(bevy_to_godot_transform(&req.transform));
        }
    }
}

pub mod plugin {
    use bevy::app::{App, FixedPreUpdate, FixedUpdate, Plugin};

    use crate::debug::cuboids::resources::{CuboidDriver, DebugCuboidRequests};
    use crate::debug::cuboids::systems::{apply_cuboids, ensure_parent};

    pub struct DebugCuboidVisualizationPlugin;
    impl Plugin for DebugCuboidVisualizationPlugin {
        fn build(&self, app: &mut App) {
            app.init_resource::<DebugCuboidRequests>()
                .insert_non_send_resource(CuboidDriver::default())
                .add_systems(FixedPreUpdate, ensure_parent)
                .add_systems(FixedUpdate, apply_cuboids);
        }
    }
}
