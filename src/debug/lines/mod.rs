pub mod resources {
    use bevy::platform::collections::HashMap;
    use bevy::prelude::Resource;
    use godot::builtin::{Color, Vector3};
    use godot::classes::{ArrayMesh, MeshInstance3D, Node, StandardMaterial3D};
    use godot::obj::Gd;

    #[derive(Clone, Copy)]
    pub struct LineConfig {
        pub always_on_top: bool,
        pub world_origin: Option<Vector3>,
    }
    impl Default for LineConfig {
        fn default() -> Self {
            Self {
                always_on_top: false,
                world_origin: None,
            }
        }
    }

    #[derive(Clone, Copy)]
    pub struct LineRequest {
        pub a: Vector3,
        pub b: Vector3,
        pub color: Color,
        pub config: LineConfig,
    }

    #[derive(Resource, Default)]
    pub struct DebugLineRequests {
        pub pending: HashMap<String, LineRequest>,
    }

    impl DebugLineRequests {
        pub fn set_line(
            &mut self,
            key: impl Into<String>,
            a: Vector3,
            b: Vector3,
            color: Color,
            config: LineConfig,
        ) {
            self.pending.insert(
                key.into(),
                LineRequest {
                    a,
                    b,
                    color,
                    config,
                },
            );
        }

        pub fn clear_line(&mut self, key: &str) {
            self.pending.remove(key);
        }

        pub fn clear_all(&mut self) {
            self.pending.clear();
        }
    }

    pub struct LineNode {
        pub mesh_instance: Gd<MeshInstance3D>,
        pub mesh: Gd<ArrayMesh>,
        pub material: Gd<StandardMaterial3D>,
    }

    #[derive(Default)]
    pub struct LineDriver {
        pub(super) parent: Option<Gd<Node>>,
        pub(super) nodes: HashMap<String, LineNode>,
    }
}

pub mod systems {
    use crate::debug::debug_manager::DebugRenderGate;
    use crate::debug::lines::resources::{DebugLineRequests, LineDriver, LineNode};
    use bevy::prelude::{NonSendMut, ResMut};
    use godot::builtin::{NodePath, PackedColorArray, PackedVector3Array, VarArray, Variant};
    use godot::classes::base_material_3d::{Flags, ShadingMode};
    use godot::classes::mesh::{ArrayType, PrimitiveType};
    use godot::classes::{ArrayMesh, Engine, MeshInstance3D, SceneTree, StandardMaterial3D};
    use godot::meta::ToGodot;
    use godot::obj::{EngineEnum, Gd, NewAlloc, NewGd, Singleton};

    pub(super) fn ensure_parent(mut driver: NonSendMut<LineDriver>) {
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

    pub(super) fn apply_lines(
        mut gate: DebugRenderGate,
        mut reqs: ResMut<DebugLineRequests>,
        mut driver: NonSendMut<LineDriver>,
    ) {
        let status = gate.get_status(1.0);

        if !status.should_rerender {
            return;
        }

        if reqs.pending.is_empty() {
            return;
        }

        let Some(mut parent) = driver.parent.as_ref().cloned() else {
            return;
        };

        let pending = std::mem::take(&mut reqs.pending);

        for (key, req) in pending {
            let entry = if let Some(entry) = driver.nodes.get(&key) {
                LineNode {
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

                let node = LineNode {
                    mesh_instance: mi.clone(),
                    mesh: mesh.clone(),
                    material: mat.clone(),
                };
                driver.nodes.insert(key.clone(), node);

                LineNode {
                    mesh_instance: mi,
                    mesh,
                    material: mat,
                }
            };

            let mut mi = entry.mesh_instance;

            if !status.is_visible {
                mi.set_visible(false);
                continue;
            }
            mi.set_visible(true);

            let mut verts = PackedVector3Array::new();
            verts.push(req.a);
            verts.push(req.b);

            let mut cols = PackedColorArray::new();
            cols.push(req.color);
            cols.push(req.color);

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
    use crate::debug::lines::resources::{DebugLineRequests, LineDriver};
    use crate::debug::lines::systems::{apply_lines, ensure_parent};
    use bevy::app::{App, FixedPreUpdate, FixedUpdate, Plugin};

    pub struct DebugLineVisualizationPlugin;
    impl Plugin for DebugLineVisualizationPlugin {
        fn build(&self, app: &mut App) {
            app.init_resource::<DebugLineRequests>()
                .insert_non_send_resource(LineDriver::default())
                .add_systems(FixedPreUpdate, ensure_parent)
                .add_systems(FixedUpdate, apply_lines);
        }
    }
}
