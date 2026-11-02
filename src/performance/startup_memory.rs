//! Samples this game process when startup completes. Values are bytes.
use memory_stats::memory_stats;

#[derive(Clone, Copy)]
pub(crate) struct Memory {
    pub godot_static: u64,
    pub resident: Option<usize>,
    pub private_commit: Option<usize>,
}

impl Memory {
    pub fn at_ready(godot_static: u64) -> Self {
        let process = memory_stats();
        Self {
            godot_static,
            resident: process.map(|sample| sample.physical_mem),
            // Only Windows defines the second counter as committed private memory.
            #[cfg(windows)]
            private_commit: process.map(|sample| sample.virtual_mem),
            #[cfg(not(windows))]
            private_commit: None,
        }
    }
}
