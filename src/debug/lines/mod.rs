// src/debug/lines/mod.rs
pub mod resources {
    use bevy::platform::collections::HashMap;
    use bevy::prelude::Resource;
    use godot::builtin::{Color, Vector3};
    use godot::classes::{MeshInstance3D, Node};
    use godot::obj::Gd;

    #[derive(Clone, Copy)]
    pub struct LineConfig {
        /// If true, disable depth test so the line is always visible.
        pub always_on_top: bool,
        /// Optional world transform origin for the mesh (simple translation).
        pub world_origin: Option<Vector3>,
    }
    impl Default for LineConfig {
        fn default() -> Self {
            Self { always_on_top: false, world_origin: None }
        }
    }

    #[derive(Clone, Copy)]
    pub struct LineRequest {
        pub a: Vector3,
        pub b: Vector3,
        pub color: Color,
        pub config: LineConfig,
    }

    /// Keyed requests (each key draws/updates a single line).
    #[derive(Resource, Default)]
    pub struct DebugLineRequests {
        pub pending: HashMap<String, LineRequest>,
    }

    impl DebugLineRequests {
        /// Set/replace a line under a stable key.
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
                LineRequest { a, b, color, config },
            );
        }

        /// Remove a specific line next frame (by key).
        pub fn clear_line(&mut self, key: &str) {
            self.pending.remove(key);
        }

        /// Clear all lines next frame.
        pub fn clear_all(&mut self) {
            self.pending.clear();
        }
    }

    /// NonSend: owns Godot handles, touched only on main thread.
    #[derive(Default)]
    pub struct LineDriver {
        pub(super) parent: Option<Gd<Node>>,
        pub(super) nodes: HashMap<String, Gd<MeshInstance3D>>,
    }
}

pub mod systems {
    use bevy::prelude::{NonSendMut, Res, ResMut};
    use godot::builtin::{NodePath, PackedColorArray, PackedVector3Array, Variant, VariantArray};
    use godot::classes::{ArrayMesh, Engine, MeshInstance3D, SceneTree, StandardMaterial3D};
    use godot::classes::base_material_3d::{Flags, ShadingMode};
    use godot::classes::mesh::{ArrayType, PrimitiveType};
    use godot::meta::ToGodot;
    use godot::obj::{EngineEnum, Gd, NewAlloc, NewGd, Singleton};
    use crate::debug::debug_manager::DebugMode;
    use crate::debug::lines::resources::{DebugLineRequests, LineDriver};

    pub(super) fn ensure_parent(mut driver: NonSendMut<LineDriver>) {
        if driver.parent.is_some() {
            return;
        }
        let engine = Engine::singleton();
        let Some(loop_obj) = engine.get_main_loop() else { return; };
        let tree = loop_obj.cast::<SceneTree>();
        let Some(mut root) = tree.get_root() else { return; };

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
        debug_res: Res<DebugMode>,
        mut reqs: ResMut<DebugLineRequests>,
        mut driver: NonSendMut<LineDriver>,
    ) {
        if reqs.pending.is_empty() {
            return;
        }

        let Some(mut parent) = driver.parent.as_ref().cloned() else { return; };

        // Take requests for this frame.
        let pending = std::mem::take(&mut reqs.pending);

        for (key, req) in pending {
            // Get/create node for this key.
            let mut mi = if let Some(n) = driver.nodes.get(&key) {
                n.clone()
            } else {
                let mut n = MeshInstance3D::new_alloc();
                n.set_name(&key);
                parent.add_child(&n);
                driver.nodes.insert(key.clone(), n.clone());
                n
            };

            if !debug_res.on {
                mi.set_visible(false);
                continue;
            }
            mi.set_visible(true);

            // Build mesh: 2 verts + per-vertex color.
            let mut verts = PackedVector3Array::new();
            verts.push(req.a);
            verts.push(req.b);

            let mut cols = PackedColorArray::new();
            cols.push(req.color);
            cols.push(req.color);

            let mut arrays = VariantArray::new();
            arrays.resize(ArrayType::MAX.ord() as usize, &Variant::nil());
            arrays.set(ArrayType::VERTEX.ord() as usize, &verts.to_variant());
            arrays.set(ArrayType::COLOR .ord() as usize, &cols.to_variant());

            let mut arr_mesh = ArrayMesh::new_gd();
            // Newer bindings take the typed enum directly:
            arr_mesh.add_surface_from_arrays(PrimitiveType::LINES, &arrays);
            mi.set_mesh(&arr_mesh);

            // Material: unshaded, vertex color as albedo, optional no depth.
            let mut mat = StandardMaterial3D::new_gd();
            mat.set_shading_mode(ShadingMode::UNSHADED);
            mat.set_flag(Flags::ALBEDO_FROM_VERTEX_COLOR, true);
            if req.config.always_on_top {
                mat.set_flag(Flags::DISABLE_DEPTH_TEST, true);
            }
            mi.set_surface_override_material(0, &mat);

            // Optional translation for the whole batch.
            if let Some(origin) = req.config.world_origin {
                let mut t = mi.get_transform();
                t.origin = origin;
                mi.set_transform(t);
            }
        }
    }
}

pub mod plugin {
    use bevy::app::{App, FixedPreUpdate, FixedUpdate, Plugin};
    use crate::debug::lines::resources::{DebugLineRequests, LineDriver};
    use crate::debug::lines::systems::{apply_lines, ensure_parent};

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
