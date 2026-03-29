pub mod nodes {
    use godot::classes::base_material_3d::{ShadingMode, TextureFilter, Transparency};
    use godot::classes::{IMeshInstance3D, MeshInstance3D, PlaneMesh, StandardMaterial3D};
    use godot::prelude::*;

    #[derive(GodotClass)]
    #[class(base=MeshInstance3D)]
    pub struct HeatmapPlane {
        #[base]
        base: Base<MeshInstance3D>,

        map_size: f32,
        material: Option<Gd<StandardMaterial3D>>,
    }

    #[godot_api]
    impl IMeshInstance3D for HeatmapPlane {
        fn init(base: Base<MeshInstance3D>) -> Self {
            Self {
                base,
                map_size: 50.0,
                material: None,
            }
        }

        fn ready(&mut self) {
            let mut plane = PlaneMesh::new_gd();
            plane.set_size(Vector2::new(self.map_size, self.map_size));
            self.base_mut().set_mesh(&plane);

            let mut mat = StandardMaterial3D::new_gd();
            mat.set_shading_mode(ShadingMode::UNSHADED);
            mat.set_transparency(Transparency::ALPHA);
            mat.set_texture_filter(TextureFilter::LINEAR);
            self.base_mut().set_material_override(&mat);
            self.material = Some(mat);
        }
    }
}

pub mod resources {
    use bevy::platform::collections::{HashMap, HashSet};
    use bevy::prelude::Resource;
    use godot::builtin::Color;
    use godot::classes::{ImageTexture, MeshInstance3D, Node, StandardMaterial3D};
    use godot::obj::Gd;

    #[derive(Clone, Copy)]
    pub enum Normalize {
        Auto,
        Fixed { min: f32, max: f32 },
    }

    #[derive(Clone)]
    pub struct HeatmapConfig {
        pub map_size: f32,
        pub world_pos: (f32, f32, f32),
        pub normalize: Normalize,
        pub upscale: i32,
        pub tint_alpha: f32,
        pub colors: (Color, Color),
    }

    impl Default for HeatmapConfig {
        fn default() -> Self {
            Self {
                map_size: 50.0,
                world_pos: (0.0, 0.1, 0.0),
                normalize: Normalize::Auto,
                upscale: 64,
                tint_alpha: 0.8,
                colors: (
                    Color::from_hsv(10.0, 80.0, 80.0),
                    Color::from_hsv(130.0, 80.0, 80.0),
                ),
            }
        }
    }

    #[derive(Resource, Default)]
    pub struct DebugHeatmapRequests {
        pub pending: HashMap<String, HeatmapRequest>,
        pub pending_clear_key_set: HashSet<String>,
    }

    #[derive(Default, Clone)]
    pub struct HeatmapRequest {
        pub columns: i32,
        pub rows: i32,
        pub data: Vec<f32>,
        pub config: HeatmapConfig,
    }

    pub struct HeatmapNode {
        pub mesh: Gd<MeshInstance3D>,
        pub material: Gd<StandardMaterial3D>,
        pub texture: Gd<ImageTexture>,
        pub last_texture_signature: Option<u64>,
        pub last_geometry_signature: Option<u64>,
    }

    #[derive(Default)]
    pub struct DebugHeatmapDriver {
        pub(super) parent: Option<Gd<Node>>,
        pub(super) nodes: HashMap<String, HeatmapNode>,
    }

    impl DebugHeatmapRequests {
        // Last operation per key within the frame wins:
        // - set_heatmap* removes a pending clear for the same key
        // - clear_heatmap removes a pending set for the same key
        pub fn set_heatmap_owned(
            &mut self,
            key: impl Into<String>,
            cols: i32,
            rows: i32,
            data: Vec<f32>,
            cfg: HeatmapConfig,
        ) {
            let key = key.into();
            assert_eq!(
                (cols as usize) * (rows as usize),
                data.len(),
                "heatmap data size mismatch"
            );

            self.pending_clear_key_set.remove(&key);

            self.pending.insert(
                key,
                HeatmapRequest {
                    columns: cols,
                    rows,
                    data,
                    config: cfg,
                },
            );
        }

        pub fn set_heatmap(
            &mut self,
            key: impl Into<String>,
            cols: i32,
            rows: i32,
            data: &[f32],
            cfg: HeatmapConfig,
        ) {
            self.set_heatmap_owned(key, cols, rows, data.to_vec(), cfg);
        }

        pub fn clear_heatmap(&mut self, key: impl Into<String>) {
            let key = key.into();
            self.pending.remove(&key);
            self.pending_clear_key_set.insert(key);
        }
    }
}

pub mod subsystem {
    use super::resources::DebugHeatmapRequests;
    pub use super::resources::HeatmapConfig;
    use bevy::ecs::system::SystemParam;
    use bevy::prelude::ResMut;

    #[derive(SystemParam)]
    pub struct DebugHeatmapSubsystem<'w> {
        requests: ResMut<'w, DebugHeatmapRequests>,
    }

    impl<'w> DebugHeatmapSubsystem<'w> {
        pub fn set_heatmap_owned(
            &mut self,
            key: impl Into<String>,
            cols: i32,
            rows: i32,
            data: Vec<f32>,
            cfg: HeatmapConfig,
        ) {
            self.requests.set_heatmap_owned(key, cols, rows, data, cfg);
        }

        pub fn set_heatmap(
            &mut self,
            key: impl Into<String>,
            cols: i32,
            rows: i32,
            data: &[f32],
            cfg: HeatmapConfig,
        ) {
            self.requests.set_heatmap(key, cols, rows, data, cfg);
        }

        pub fn clear_heatmap(&mut self, key: impl Into<String>) {
            self.requests.clear_heatmap(key);
        }
    }
}

pub mod systems {
    use bevy::platform::collections::hash_map::Entry;
    use bevy::prelude::{NonSendMut, ResMut};
    use godot::builtin::{NodePath, PackedByteArray, Vector2, Vector3};
    use godot::classes::base_material_3d::{
        ShadingMode, TextureFilter, TextureParam, Transparency,
    };
    use godot::classes::image::Format;
    use godot::classes::{
        Engine, Image, ImageTexture, MeshInstance3D, Node, PlaneMesh, SceneTree, StandardMaterial3D,
    };
    use godot::obj::{Gd, NewAlloc, NewGd, Singleton};

    use crate::debug::debug_manager::DebugRenderGateSubsystem;
    use crate::debug::heatmap::resources::{
        DebugHeatmapDriver, DebugHeatmapRequests, HeatmapNode, HeatmapRequest, Normalize,
    };
    use crate::prelude::EDebugState;
    use std::hash::{Hash, Hasher};

    fn hash_texture_request(request: &HeatmapRequest) -> u64 {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();

        request.columns.hash(&mut hasher);
        request.rows.hash(&mut hasher);

        request.config.tint_alpha.to_bits().hash(&mut hasher);
        request.config.upscale.hash(&mut hasher);

        request.config.colors.0.r.to_bits().hash(&mut hasher);
        request.config.colors.0.g.to_bits().hash(&mut hasher);
        request.config.colors.0.b.to_bits().hash(&mut hasher);
        request.config.colors.0.a.to_bits().hash(&mut hasher);
        request.config.colors.1.r.to_bits().hash(&mut hasher);
        request.config.colors.1.g.to_bits().hash(&mut hasher);
        request.config.colors.1.b.to_bits().hash(&mut hasher);
        request.config.colors.1.a.to_bits().hash(&mut hasher);

        match request.config.normalize {
            Normalize::Auto => 0_u8.hash(&mut hasher),
            Normalize::Fixed { min, max } => {
                1_u8.hash(&mut hasher);
                min.to_bits().hash(&mut hasher);
                max.to_bits().hash(&mut hasher);
            }
        }

        request.data.len().hash(&mut hasher);
        for value in &request.data {
            value.to_bits().hash(&mut hasher);
        }

        hasher.finish()
    }

    fn hash_geometry_request(request: &HeatmapRequest) -> u64 {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        request.config.map_size.to_bits().hash(&mut hasher);
        request.config.world_pos.0.to_bits().hash(&mut hasher);
        request.config.world_pos.1.to_bits().hash(&mut hasher);
        request.config.world_pos.2.to_bits().hash(&mut hasher);
        hasher.finish()
    }

    pub(super) fn ensure_driver_parent(mut driver: NonSendMut<DebugHeatmapDriver>) {
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
        let parent: Gd<Node> = if let Some(n) = root.try_get_node_as::<Node>(&path) {
            n
        } else {
            let mut n = Node::new_alloc();
            n.set_name("BevyDebug");
            root.add_child(&n);
            n
        };

        driver.parent = Some(parent);
    }

    pub(super) fn apply_heatmaps(
        mut gate: DebugRenderGateSubsystem,
        mut reqs: ResMut<DebugHeatmapRequests>,
        mut driver: NonSendMut<DebugHeatmapDriver>,
    ) {
        let pending_clear_key_set = std::mem::take(&mut reqs.pending_clear_key_set);
        for key in pending_clear_key_set {
            reqs.pending.remove(&key);
            if let Some(node) = driver.nodes.remove(&key) {
                let mut mesh = node.mesh;
                mesh.queue_free();
            }
        }

        let status = gate.get_status(EDebugState::Colliders, 1.0 / 32.0);

        if !status.should_rerender {
            return;
        }

        let Some(mut parent) = driver.parent.as_ref().cloned() else {
            return;
        };

        for node in driver.nodes.values() {
            let mut mesh = node.mesh.clone();
            mesh.set_visible(status.is_visible);
        }

        let pending = std::mem::take(&mut reqs.pending);

        for (key, request) in pending {
            let entry = match driver.nodes.entry(key.clone()) {
                Entry::Occupied(entry) => entry.into_mut(),
                Entry::Vacant(entry) => {
                    let mut mesh_instance = MeshInstance3D::new_alloc();

                    let mut plane = PlaneMesh::new_gd();
                    plane.set_size(Vector2::new(
                        request.config.map_size,
                        request.config.map_size,
                    ));
                    mesh_instance.set_mesh(&plane);

                    let mut material = StandardMaterial3D::new_gd();
                    material.set_shading_mode(ShadingMode::UNSHADED);
                    material.set_transparency(Transparency::ALPHA);
                    material.set_texture_filter(TextureFilter::LINEAR);

                    let texture = ImageTexture::new_gd();
                    material.set_texture(TextureParam::ALBEDO, &texture);
                    mesh_instance.set_material_override(&material);

                    mesh_instance.set_name(&key);
                    parent.add_child(&mesh_instance);

                    entry.insert(HeatmapNode {
                        mesh: mesh_instance,
                        material,
                        texture,
                        last_texture_signature: None,
                        last_geometry_signature: None,
                    })
                }
            };

            let mut mesh_instance = entry.mesh.clone();
            mesh_instance.set_visible(status.is_visible);

            let geometry_signature = hash_geometry_request(&request);
            if entry.last_geometry_signature != Some(geometry_signature) {
                let (x, y, z) = request.config.world_pos;
                let mut transform = mesh_instance.get_transform();
                transform.origin = Vector3::new(x, y, z);
                mesh_instance.set_transform(transform);

                let mut plane = PlaneMesh::new_gd();
                plane.set_size(Vector2::new(
                    request.config.map_size,
                    request.config.map_size,
                ));
                mesh_instance.set_mesh(&plane);

                entry.last_geometry_signature = Some(geometry_signature);
            }

            let texture_signature = hash_texture_request(&request);
            if entry.last_texture_signature == Some(texture_signature) {
                continue;
            }

            let (boundary_low, boundary_high) = match request.config.normalize {
                Normalize::Fixed { min, max } if min < max => (min, max),
                _ => {
                    let mut min = f32::INFINITY;
                    let mut max = f32::NEG_INFINITY;
                    for &v in &request.data {
                        min = min.min(v);
                        max = max.max(v);
                    }
                    if !min.is_finite() || !max.is_finite() || (max - min).abs() < 1e-12 {
                        (0.0, 1.0)
                    } else {
                        (min, max)
                    }
                }
            };
            let inv_span = 1.0 / (boundary_high - boundary_low).max(1e-12);

            let src_cols = request.columns.max(1);
            let src_rows = request.rows.max(1);

            let (color_min, color_max) = request.config.colors;
            let tint_alpha = request.config.tint_alpha;

            let mut bytes = vec![0u8; (src_cols as usize) * (src_rows as usize) * 4];

            for y in 0..src_rows {
                for x in 0..src_cols {
                    let idx = (y * src_cols + x) as usize;
                    let v = *request.data.get(idx).unwrap_or(&0.0);
                    let t = ((v - boundary_low) * inv_span).clamp(0.0, 1.0);

                    let mut c = color_min.lerp(color_max, t as f64);
                    c.a = (t * tint_alpha).clamp(0.0, 1.0);

                    let r = (c.r.clamp(0.0, 1.0) * 255.0).round() as u8;
                    let g = (c.g.clamp(0.0, 1.0) * 255.0).round() as u8;
                    let b = (c.b.clamp(0.0, 1.0) * 255.0).round() as u8;
                    let a = (c.a.clamp(0.0, 1.0) * 255.0).round() as u8;

                    let o = idx * 4;
                    bytes[o] = r;
                    bytes[o + 1] = g;
                    bytes[o + 2] = b;
                    bytes[o + 3] = a;
                }
            }

            let mut image = Image::create_from_data(
                src_cols,
                src_rows,
                false,
                Format::RGBA8,
                &PackedByteArray::from(bytes),
            )
            .expect("failed to create heatmap image from data");

            let upscale = request.config.upscale.max(1);
            if upscale > 1 {
                image.resize(src_cols * upscale, src_rows * upscale);
            }

            let mut tex = entry.texture.clone();
            tex.set_image(&image);
            entry.last_texture_signature = Some(texture_signature);
        }
    }
}

pub mod plugin {
    use bevy::app::{App, FixedPreUpdate, FixedUpdate, Plugin};
    use bevy_godot4::debug::heatmap::systems::ensure_driver_parent;

    use crate::debug::heatmap::resources::{DebugHeatmapDriver, DebugHeatmapRequests};
    use crate::debug::heatmap::systems::apply_heatmaps;

    pub struct DebugHeatmapVisualizationPlugin;

    impl Plugin for DebugHeatmapVisualizationPlugin {
        fn build(&self, app: &mut App) {
            app.init_resource::<DebugHeatmapRequests>()
                .insert_non_send_resource(DebugHeatmapDriver::default())
                .add_systems(FixedPreUpdate, ensure_driver_parent)
                .add_systems(FixedUpdate, apply_heatmaps);
        }
    }
}
