//! Optional bridge-owned debug state and visualization helpers.

pub(crate) mod cuboids;
pub mod debug_manager;
pub(crate) mod heatmap;
pub(crate) mod paths;
pub(crate) mod visibility;

pub mod plugins {
    use super::cuboids::plugin::DebugCuboidVisualizationPlugin;
    use super::debug_manager::DebugModeBridgePlugin;
    use super::heatmap::plugin::DebugHeatmapVisualizationPlugin;
    use super::paths::plugin::DebugPathVisualizationPlugin;
    use bevy::prelude::*;

    /// Installs all bridge-provided debug visualization plugins.
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
