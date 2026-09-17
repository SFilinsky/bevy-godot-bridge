//! Rules for changing data between Rust and Godot objects.
//!
//! A `DataTransferConfig` tells the bridge how to copy one Rust value into one
//! Godot object, and how to copy it back. Different macros use this same rule
//! for different jobs.

#[cfg(feature = "bridge-transport-profiling")]
use std::cell::{Cell, RefCell};
#[cfg(feature = "bridge-transport-profiling")]
use std::time::{Duration, Instant};

use crate::import::subsystems::IdentitySubsystem;
use godot::obj::NewGd;
use godot::prelude::{Gd, GodotClass, Node, RefCounted};

#[cfg(feature = "bridge-transport-profiling")]
thread_local! {
    static OUTBOUND_DTO_BUILD_TIMING: RefCell<Option<OutboundDtoBuildTiming>> = const { RefCell::new(None) };
    static OUTBOUND_DTO_BUILD_DEPTH: Cell<u32> = const { Cell::new(0) };
}

/// Detailed timing for DTO construction during one outbound queue drain.
#[cfg(feature = "bridge-transport-profiling")]
#[derive(Default)]
pub(crate) struct OutboundDtoBuildTiming {
    pub object_creation: Duration,
    pub field_copy: Duration,
    pub value_count: u32,
}

/// Keeps one outbound queue drain's DTO timings separate from other DTO work.
#[cfg(feature = "bridge-transport-profiling")]
pub(crate) struct OutboundDtoBuildTimingScope {
    previous_timing: Option<OutboundDtoBuildTiming>,
    has_finished: bool,
}

#[cfg(feature = "bridge-transport-profiling")]
impl OutboundDtoBuildTimingScope {
    /// Starts collecting timings for DTOs created by the current queue drain.
    pub(crate) fn begin() -> Self {
        let previous_timing = OUTBOUND_DTO_BUILD_TIMING.with(|timing| {
            timing.borrow_mut().replace(OutboundDtoBuildTiming::default())
        });
        Self {
            previous_timing,
            has_finished: false,
        }
    }

    /// Stops collection and returns the combined timing for this queue drain.
    pub(crate) fn finish(mut self) -> OutboundDtoBuildTiming {
        let timing = OUTBOUND_DTO_BUILD_TIMING.with(|active_timing| {
            active_timing.borrow_mut().take().unwrap_or_default()
        });
        let previous_timing = self.previous_timing.take();
        OUTBOUND_DTO_BUILD_TIMING.with(|active_timing| {
            *active_timing.borrow_mut() = previous_timing;
        });
        self.has_finished = true;
        timing
    }
}

#[cfg(feature = "bridge-transport-profiling")]
impl Drop for OutboundDtoBuildTimingScope {
    fn drop(&mut self) {
        if self.has_finished {
            return;
        }
        let previous_timing = self.previous_timing.take();
        OUTBOUND_DTO_BUILD_TIMING.with(|active_timing| {
            *active_timing.borrow_mut() = previous_timing;
        });
    }
}

#[cfg(feature = "bridge-transport-profiling")]
struct OutboundDtoBuildDepthGuard {
    is_top_level: bool,
}

#[cfg(feature = "bridge-transport-profiling")]
impl OutboundDtoBuildDepthGuard {
    fn enter() -> Self {
        let is_top_level = OUTBOUND_DTO_BUILD_DEPTH.with(|depth| {
            let is_top_level = depth.get() == 0;
            depth.set(depth.get().saturating_add(1));
            is_top_level
        });
        Self { is_top_level }
    }
}

#[cfg(feature = "bridge-transport-profiling")]
impl Drop for OutboundDtoBuildDepthGuard {
    fn drop(&mut self) {
        OUTBOUND_DTO_BUILD_DEPTH.with(|depth| depth.set(depth.get().saturating_sub(1)));
    }
}

#[cfg(feature = "bridge-transport-profiling")]
fn measure_outbound_dto_build<T>(
    stage: OutboundDtoBuildStage,
    operation: impl FnOnce() -> T,
) -> T {
    let has_active_timing = OUTBOUND_DTO_BUILD_TIMING.with(|timing| timing.borrow().is_some());
    if !has_active_timing {
        return operation();
    }

    let started_at = Instant::now();
    let result = operation();
    let duration = started_at.elapsed();
    OUTBOUND_DTO_BUILD_TIMING.with(|timing| {
        let mut active_timing = timing.borrow_mut();
        let Some(timing) = active_timing.as_mut() else {
            return;
        };
        match stage {
            OutboundDtoBuildStage::ObjectCreation => timing.object_creation += duration,
            OutboundDtoBuildStage::FieldCopy => timing.field_copy += duration,
        }
    });
    result
}

#[cfg(feature = "bridge-transport-profiling")]
#[derive(Clone, Copy)]
enum OutboundDtoBuildStage {
    ObjectCreation,
    FieldCopy,
}

/// Describes how one Rust type and one Godot type copy data to each other.
///
/// A DTO is a small Godot object that carries data across the bridge. Methods
/// that write a DTO send data to Godot. Methods that write Rust data receive it
/// from Godot. A macro uses only the direction it needs.
pub trait DataTransferConfig {
    type DataType: Default + Sized;
    type DtoType: GodotClass + NewGd;

    /// Says whether this config uses the default [`Self::from_data`] method.
    ///
    /// Export profiling uses this to label the full top-level DTO build. A
    /// config that writes its own `from_data` method must set this to `false`.
    const USES_DEFAULT_OUTBOUND_DTO_BUILD: bool = true;

    /// Copies a Rust value into an existing Godot data object.
    fn update_dto(
        dto: &mut Gd<Self::DtoType>,
        data: &Self::DataType,
        identity: &mut IdentitySubsystem,
    );

    /// Copies a Godot data object into an existing Rust value.
    fn update_data(
        dto: &Gd<Self::DtoType>,
        data: &mut Self::DataType,
        identity: &mut IdentitySubsystem,
    );

    /// Makes a new Rust value from a Godot data object.
    fn from_dto(dto: &Gd<Self::DtoType>, identity: &mut IdentitySubsystem) -> Self::DataType {
        let mut data = Self::DataType::default();
        Self::update_data(dto, &mut data, identity);
        data
    }

    /// Makes a new Godot data object from a Rust value.
    fn from_data(data: &Self::DataType, identity: &mut IdentitySubsystem) -> Gd<Self::DtoType> {
        #[cfg(feature = "bridge-transport-profiling")]
        {
            let depth_guard = OutboundDtoBuildDepthGuard::enter();
            let mut dto = if depth_guard.is_top_level {
                measure_outbound_dto_build(OutboundDtoBuildStage::ObjectCreation, || {
                    Self::DtoType::new_gd()
                })
            } else {
                Self::DtoType::new_gd()
            };
            if depth_guard.is_top_level {
                measure_outbound_dto_build(OutboundDtoBuildStage::FieldCopy, || {
                    Self::update_dto(&mut dto, data, identity);
                });
                OUTBOUND_DTO_BUILD_TIMING.with(|timing| {
                    if let Some(timing) = timing.borrow_mut().as_mut() {
                        timing.value_count += 1;
                    }
                });
            } else {
                Self::update_dto(&mut dto, data, identity);
            }
            return dto;
        }

        #[cfg(not(feature = "bridge-transport-profiling"))]
        {
        let mut dto = Self::DtoType::new_gd();
        Self::update_dto(&mut dto, data, identity);
        dto
        }
    }
}

/// Builds a Godot data object from a Godot node.
///
/// Import macros use this to read values that a designer set in the scene.
pub trait BuildsDto<Dto>
where
    Dto: GodotClass,
{
    fn build_dto(&self) -> Gd<Dto>;
}

/// Names the Godot node that reads scene data for this data object.
pub trait WithGatherer: GodotClass<Base = RefCounted> {
    type Gatherer: GodotClass<Base = Node> + BuildsDto<Self>;
}

/// Names the Godot node that stores this kind of entity data.
///
/// The node keeps the current and previous values. It also lets a Godot script
/// ask whether an entity has this data without using dynamic property names.
pub trait WithStateNode: GodotClass {
    type StateNode: GodotClass<Base = Node>;
}
