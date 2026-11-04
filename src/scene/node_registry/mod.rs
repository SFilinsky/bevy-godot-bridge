//! Register services once and find them within one level or an explicitly supplied owner.
//!
//! See `README.md` for the nine-method Rust/GDScript API and entry/exit rules.
mod api;
mod storage;

pub use api::{NodeRegistry, RegistryError};
