//! Checks which builds may measure and whether this play session requested startup timing.
//! Worker threads read the cached permission without calling Godot.
use godot::classes::{Engine, Os, ProjectSettings};
use godot::obj::Singleton;
use std::env;
use std::sync::atomic::{AtomicBool, Ordering};

const CAPABLE: bool = cfg!(any(
    all(debug_assertions, feature = "dev-observability"),
    feature = "profiled-release"
));
static ENABLED: AtomicBool = AtomicBool::new(false);

pub(super) fn collecting() -> bool {
    CAPABLE && ENABLED.load(Ordering::Relaxed)
}

pub(super) fn enable_collection() -> bool {
    let enabled = allowed();
    ENABLED.store(enabled, Ordering::Relaxed);
    enabled
}

/// Checks whether this native build and Godot host may collect measurements.
pub(crate) fn allowed() -> bool {
    if !CAPABLE {
        return false;
    }
    let os = Os::singleton();
    os.has_feature("debug")
        || (cfg!(feature = "profiled-release") && os.has_feature("profiled_release"))
}

/// Checks whether this play session asked to record startup from level entry.
pub(crate) fn requested() -> bool {
    allowed()
        && !Engine::singleton().is_editor_hint()
        && (env::var_os("EQUILIBRIA_BENCHMARK_OUTPUT_FILE").is_some()
            || Os::singleton()
                .get_cmdline_user_args()
                .as_slice()
                .iter()
                .any(|arg| arg.to_string() == "--measure-startup")
            || ProjectSettings::singleton()
                .get_setting("debug/observability/measure_play")
                .try_to::<bool>()
                .unwrap_or(false))
}
