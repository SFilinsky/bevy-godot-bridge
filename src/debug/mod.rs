pub(crate) mod heatmap;
pub(crate) mod lines;
pub(crate) mod cuboids;
mod debug_manager;

pub mod plugins {
    use bevy::prelude::*;
    use super::debug_manager::DebugModeBridgePlugin;
    use super::cuboids::plugin::DebugCuboidVisualizationPlugin;
    use super::heatmap::plugin::DebugHeatmapVisualizationPlugin;
    use super::lines::plugin::DebugLineVisualizationPlugin;

    pub struct DebugVisualizationPlugin;
    impl Plugin for DebugVisualizationPlugin {
        fn build(&self, app: &mut App) {
            app.add_plugins((
                DebugCuboidVisualizationPlugin,
                DebugHeatmapVisualizationPlugin,
                DebugLineVisualizationPlugin,
                DebugModeBridgePlugin
            ));
        }
    }
}
