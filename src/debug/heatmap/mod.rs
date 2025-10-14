pub mod nodes {
    use godot::classes::{IMeshInstance3D, MeshInstance3D, PlaneMesh, StandardMaterial3D};
    use godot::classes::base_material_3d::{ShadingMode, TextureFilter, Transparency};
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
            self.base_mut().set_surface_override_material(0, &mat.clone());
            self.material = Some(mat);
        }
    }
}

pub mod resources {
    use bevy::platform::collections::HashMap;
    use bevy::prelude::Resource;
    use godot::builtin::Color;
    use godot::classes::{MeshInstance3D, Node};
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
        pub colors: (Color, Color)
    }

    impl Default for HeatmapConfig {
        fn default() -> Self {
            Self {
                map_size: 50.0,
                world_pos: (0.0, 0.1, 0.0),
                normalize: Normalize::Auto,
                upscale: 64,
                tint_alpha: 0.8,
                colors: (Color::from_hsv(10.0, 80.0, 80.0), Color::from_hsv(130.0, 80.0, 80.0))
            }
        }
    }

    #[derive(Resource, Default)]
    pub struct DebugHeatmapRequests {
        pub pending: HashMap<String, HeatmapRequest>,
    }

    #[derive(Default, Clone)]
    pub struct HeatmapRequest {
        pub columns: i32,
        pub rows: i32,
        pub data: Vec<f32>,
        pub config: HeatmapConfig,
    }


    // ---- NonSend driver: owns Gd handles, only touched on the main thread ----
    #[derive(Default)]
    pub struct DebugHeatmapDriver {
        pub(super) parent: Option<Gd<Node>>,
        pub(super) nodes: HashMap<String, Gd<MeshInstance3D>>,
    }

    impl DebugHeatmapRequests {
        /// Queue (or refresh) a heatmap under a stable key.
        pub fn set_heatmap(
            &mut self,
            key: impl Into<String>,
            cols: i32,
            rows: i32,
            data: &[f32],
            cfg: HeatmapConfig,
        ) {
            let key = key.into();
            assert_eq!(
                (cols as usize) * (rows as usize),
                data.len(),
                "heatmap data size mismatch"
            );
            self.pending.insert(
                key,
                HeatmapRequest {
                    columns: cols,
                    rows,
                    data: data.to_vec(),
                    config: cfg,
                },
            );
        }
    }
}

pub mod systems {
    use bevy::prelude::{NonSendMut, ResMut};
    use godot::builtin::{NodePath, Vector2, Vector3};
    use godot::classes::{Engine, Image, ImageTexture, MeshInstance3D, Node, PlaneMesh, SceneTree, StandardMaterial3D};
    use godot::classes::base_material_3d::{ShadingMode, TextureParam, Transparency};
    use godot::classes::image::Format;
    use godot::obj::{Gd, NewAlloc, NewGd, Singleton};
    use crate::debug::heatmap::resources::{DebugHeatmapDriver, DebugHeatmapRequests, Normalize};

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

        // Find or create "BevyDebug"
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
        mut reqs: ResMut<DebugHeatmapRequests>,
        mut driver: NonSendMut<DebugHeatmapDriver>,
    ) {
        if reqs.pending.is_empty() {
            return;
        }
        let Some(mut parent) = driver.parent.as_ref().cloned() else {
            return;
        };

        let pending = std::mem::take(&mut reqs.pending);

        for (key, request) in pending {
            let mut mesh_instance = if let Some(node) = driver.nodes.get(&key) {
                node.clone()
            } else {
                let mut mesh_instance = MeshInstance3D::new_alloc();


                let mut plane = PlaneMesh::new_gd();
                plane.set_size(Vector2::new(request.config.map_size, request.config.map_size));
                mesh_instance.set_mesh(&plane);


                let mut material = StandardMaterial3D::new_gd();
                material.set_shading_mode(ShadingMode::UNSHADED);
                material.set_transparency(Transparency::ALPHA);
                mesh_instance.set_surface_override_material(0, &material);

                mesh_instance.set_name(&key);
                parent.add_child(&mesh_instance);
                driver.nodes.insert(key.clone(), mesh_instance.clone());
                mesh_instance
            };

            {
                let (x,y,z) = request.config.world_pos;
                let mut transform = mesh_instance.get_transform();
                transform.origin = Vector3::new(x, y, z);
                mesh_instance.set_transform(transform);
            }

            let columns = request.columns.max(1);
            let rows = request.rows.max(1);

            let (boundary_low, boundary_high) = match request.config.normalize {
                Normalize::Fixed { min, max } if min < max => (min, max),
                _ => {
                    let mut min = f32::INFINITY;
                    let mut max = f32::NEG_INFINITY;
                    for &v in &request.data {
                        min = min.min(v);
                        max = max.max(v);
                    }
                    match !min.is_finite() || !max.is_finite() || (max - min).abs() < 1e-12 {
                        true => (0.0, 1.0),
                        false => (min, max)
                    }
                }
            };
            let inv_span = 1.0 / (boundary_high - boundary_low).max(1e-12);

            let mut image = Image::create(columns, rows, false, Format::RGBA8).unwrap();
            for y in 0..rows {
                for x in 0..columns {
                    let array_index = (y * columns + x) as usize;
                    let value = *request.data.get(array_index).unwrap_or(&0.0);
                    let value_normalized = ((value - boundary_low) * inv_span).clamp(0.0, 1.0);
                    let (color_min, color_max) = request.config.colors;
                    let color = color_min.lerp(color_max, value_normalized as f64).with_alpha(value_normalized * request.config.tint_alpha);
                    image.set_pixel(x, y, color);
                }
            }
            if request.config.upscale > 1 {
                image.resize(columns * request.config.upscale, rows * request.config.upscale);
            }


            let mut tex = ImageTexture::new_gd();
            tex.set_image(&image);
            if let Some(mat) = mesh_instance.get_surface_override_material(0) {
                if let Ok(mut stdmat) = mat.try_cast::<StandardMaterial3D>() {
                    stdmat.set_texture(TextureParam::ALBEDO, &tex);
                    mesh_instance.set_surface_override_material(0, &stdmat);
                }
            }
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