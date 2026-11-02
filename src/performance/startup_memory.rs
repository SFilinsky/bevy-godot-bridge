//! Samples this game process when startup completes. Values are bytes.
#[derive(Clone, Copy)]
pub(crate) struct Memory {
    pub godot_static: u64,
    pub resident: Option<usize>,
    pub private_commit: Option<usize>,
}

impl Memory {
    pub fn at_ready(godot_static: u64) -> Self {
        let process = process_bytes();
        Self { godot_static, resident: process.map(|p| p.0), private_commit: process.map(|p| p.1) }
    }
}

#[cfg(windows)]
fn process_bytes() -> Option<(usize, usize)> {
    use std::mem::size_of;
    use windows_sys::Win32::System::ProcessStatus::{GetProcessMemoryInfo, PROCESS_MEMORY_COUNTERS_EX};
    use windows_sys::Win32::System::Threading::GetCurrentProcess;
    let size = size_of::<PROCESS_MEMORY_COUNTERS_EX>() as u32;
    let mut counters = PROCESS_MEMORY_COUNTERS_EX { cb: size, ..Default::default() };
    // SAFETY: Windows borrows the current process handle and fills exactly this buffer.
    let ok = unsafe { GetProcessMemoryInfo(GetCurrentProcess(), (&raw mut counters).cast(), size) };
    (ok != 0).then_some((counters.WorkingSetSize, counters.PrivateUsage))
}

#[cfg(not(windows))]
fn process_bytes() -> Option<(usize, usize)> { None }
