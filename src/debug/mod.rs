pub(crate) mod cuboids;
pub mod debug_manager;
pub(crate) mod heatmap;
pub(crate) mod paths;

pub mod plugins {
    use super::cuboids::plugin::DebugCuboidVisualizationPlugin;
    use super::debug_manager::DebugModeBridgePlugin;
    use super::heatmap::plugin::DebugHeatmapVisualizationPlugin;
    use super::paths::plugin::DebugPathVisualizationPlugin;
    use bevy::prelude::*;

    pub struct DebugVisualizationPlugin;
    impl Plugin for DebugVisualizationPlugin {
        fn build(&self, app: &mut App) {
            app.add_plugins((
                DebugCuboidVisualizationPlugin,
                DebugHeatmapVisualizationPlugin,
                DebugPathVisualizationPlugin,
                DebugModeBridgePlugin,
            ));
        }
    }
}
