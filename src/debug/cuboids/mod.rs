pub mod resources {
    use bevy::math::Vec3;
    use bevy::platform::collections::HashMap;
    use bevy::prelude::{Resource, Transform};
    use godot::builtin::{Color};
    use godot::classes::{MeshInstance3D, Node};
    use godot::obj::Gd;



    #[derive(Clone, Copy, Debug)]
    pub struct CuboidConfig {
        /// If true, disable depth test so the box is always visible.
        pub always_on_top: bool,
        /// If true, render as wireframe (edges only).
        pub wireframe: bool,
        /// If true, use unshaded material (ignores lights).
        pub unshaded: bool,
        /// If true, use alpha transparency (color.a is respected).
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
        /// World transform (position + rotation + uniform/non-uniform scale via mesh size below).
        pub transform: Transform,
        /// Half extents in local space (x,y,z). Final box size = half_extents * 2.
        pub half_extents: Vec3,
        /// Base color (alpha respected if `transparent` is true).
        pub color: Color,
        pub config: CuboidConfig,
    }

    /// Keyed requests (each key draws/updates a single cuboids).
    #[derive(Resource, Default)]
    pub struct DebugCuboidRequests {
        pub pending: HashMap<String, CuboidRequest>,
        /// For deleting by key this frame (optional helper).
        pub to_clear: Vec<String>,
        /// If true, clears all cuboids this frame (optional helper).
        pub clear_all: bool,
    }

    impl DebugCuboidRequests {
        /// Set/replace a cuboids under a stable key.
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
                CuboidRequest { transform, half_extents, color, config },
            );
        }

        /// Remove a specific cuboids next frame.
        pub fn clear_cuboid(&mut self, key: impl Into<String>) {
            self.to_clear.push(key.into());
        }

        /// Remove all cuboids next frame.
        pub fn clear_all(&mut self) {
            self.clear_all = true;
        }
    }

    /// NonSend: owns Godot handles, touched only on main thread.
    #[derive(Default)]
    pub struct CuboidDriver {
        pub(super) parent: Option<Gd<Node>>,
        pub(super) nodes: HashMap<String, Gd<MeshInstance3D>>,
    }
}

pub mod systems {
    use bevy::prelude::{NonSendMut, Res, ResMut, Transform};
    use godot::builtin::{Basis, NodePath, Quaternion, Transform3D, Vector3};
    use godot::classes::{
        BoxMesh, Engine, MeshInstance3D, SceneTree, StandardMaterial3D,
    };
    use godot::classes::base_material_3d::{CullMode, Flags, ShadingMode, Transparency};
    use godot::obj::{Gd, NewAlloc, NewGd, Singleton};
    use bevy_godot4::debug::debug_manager::DebugMode;
    use super::resources::{CuboidDriver, DebugCuboidRequests};

    pub fn bevy_to_godot_transform(
        t: &Transform
    ) -> Transform3D {
        // Build Godot Basis from Bevy quaternion.
        let rot = Quaternion::new(t.rotation.x, t.rotation.y, t.rotation.z, t.rotation.w);
        let basis = Basis::from_quaternion(rot);

        // Origin from Bevy translation.
        let origin = Vector3::new(t.translation.x, t.translation.y, t.translation.z);

        // Final transform (no scale inside).
        Transform3D::new(basis, origin)
    }

    pub(super) fn ensure_parent(mut driver: NonSendMut<CuboidDriver>) {
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

    pub(super) fn apply_cuboids(
        debug_res: Res<DebugMode>,
        mut reqs: ResMut<DebugCuboidRequests>,
        mut driver: NonSendMut<CuboidDriver>,
    ) {
        let Some(mut parent) = driver.parent.as_ref().cloned() else { return; };

        // Clear all requested nodes.
        if reqs.clear_all {
            for (_, n) in driver.nodes.drain() {
                parent.remove_child(&n);
            }
            reqs.clear_all = false;
        } else {
            // Clear by keys.
            for key in reqs.to_clear.drain(..) {
                if let Some(n) = driver.nodes.remove(&key) {
                    parent.remove_child(&n);
                }
            }
        }

        if reqs.pending.is_empty() {
            return;
        }

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

            // Build/refresh the mesh.
            let mut mesh = BoxMesh::new_gd();
            // BoxMesh::set_size expects the full size, so multiply half extents by 2.
            let size = Vector3::new(req.half_extents.x * 2.0, req.half_extents.y * 2.0, req.half_extents.z * 2.0);
            mesh.set_size(size);
            mi.set_mesh(&mesh);

            // Material setup.
            let mut mat = StandardMaterial3D::new_gd();
            if req.config.unshaded {
                mat.set_shading_mode(ShadingMode::UNSHADED);
            }
            if req.config.transparent || (req.color.a < 0.999) {
                mat.set_transparency(Transparency::ALPHA);
            }
            // if req.config.wireframe {
            //     mat.set_flag(Flags::USE_WIREFRAME, true);
            // }
            if req.config.always_on_top {
                mat.set_flag(Flags::DISABLE_DEPTH_TEST, true);
            }
            // Albedo color (alpha is respected when transparency is enabled).
            mat.set_albedo(req.color);
            // Optional: ensure vertex color is not interfering.
            mat.set_flag(Flags::ALBEDO_FROM_VERTEX_COLOR, false);
            // Optional: make it double-sided to see from inside.
            mat.set_cull_mode(CullMode::DISABLED);

            mi.set_surface_override_material(0, &mat);

            // World transform (position + rotation). Scale comes from mesh size above.
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
