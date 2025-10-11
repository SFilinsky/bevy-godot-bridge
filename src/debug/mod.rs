pub(crate) mod heatmap;

pub mod plugins {
    use bevy::prelude::*;
    use crate::debug::heatmap::plugin::DebugHeatmapVisualizationPlugin;

    pub struct DebugVisualizationPlugin;
    impl Plugin for DebugVisualizationPlugin {
        fn build(&self, app: &mut App) {
            app.add_plugins(DebugHeatmapVisualizationPlugin);
        }
    }
}