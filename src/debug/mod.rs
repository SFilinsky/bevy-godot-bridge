pub(crate) mod heatmap;
pub(crate) mod lines;

pub mod plugins {
    use bevy::prelude::*;
    use crate::debug::heatmap::plugin::DebugHeatmapVisualizationPlugin;
    use crate::debug::lines::plugin::DebugLineVisualizationPlugin;

    pub struct DebugVisualizationPlugin;
    impl Plugin for DebugVisualizationPlugin {
        fn build(&self, app: &mut App) {
            app.add_plugins(DebugHeatmapVisualizationPlugin)
            .add_plugins(DebugLineVisualizationPlugin);
        }
    }
}