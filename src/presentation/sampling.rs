//! Calculate a visual sample from gathered values, without reading scene objects.
pub trait Interpolate: Copy {
    fn interpolate(start: Self, end: Self, weight: f64) -> Self;
}

pub(super) struct TransitionState<T> {
    pub start: T,
    pub end: T,
    pub duration: f64,
    pub elapsed: f64,
    pub has_published: bool,
    pub last_clock: f64,
}

pub(super) struct Sample<T> {
    pub elapsed: f64,
    pub last_clock: f64,
    pub value: Option<T>,
}

pub(super) fn sample<T: Interpolate>(
    state: &TransitionState<T>,
    clock: f64,
    eligible: bool,
) -> Sample<T> {
    // Paused owners consume clock time without progressing their visual transition.
    let elapsed_delta = (clock - state.last_clock).max(0.0);
    let mut result = Sample {
        elapsed: state.elapsed,
        last_clock: clock,
        value: None,
    };
    if !eligible || (elapsed_delta == 0.0 && state.duration > 0.0 && state.has_published) {
        return result;
    }
    // New requests publish their start immediately; zero duration publishes the end.
    result.elapsed = (state.elapsed + elapsed_delta).min(state.duration);
    let weight = if state.duration > 0.0 {
        result.elapsed / state.duration
    } else {
        1.0
    };
    result.value = Some(T::interpolate(state.start, state.end, weight));
    result
}

#[cfg(test)]
mod tests {
    use super::{Interpolate, TransitionState, sample};

    impl Interpolate for f64 {
        fn interpolate(start: Self, end: Self, weight: f64) -> Self {
            start + (end - start) * weight
        }
    }

    fn state() -> TransitionState<f64> {
        TransitionState {
            start: 0.0,
            end: 10.0,
            duration: 1.0,
            elapsed: 0.0,
            has_published: false,
            last_clock: 0.0,
        }
    }

    #[test]
    fn paused_time_does_not_advance_the_transition() {
        let mut state = state();
        let paused = sample(&state, 2.0, false);
        assert!(paused.value.is_none());
        state.elapsed = paused.elapsed;
        state.last_clock = paused.last_clock;
        let resumed = sample(&state, 2.5, true);
        assert_eq!(resumed.value.unwrap(), 5.0);
    }

    #[test]
    fn new_request_publishes_start_without_advancing_time() {
        let mut state = state();
        assert_eq!(sample(&state, 0.0, true).value.unwrap(), 0.0);
        state.has_published = true;
        assert!(sample(&state, 0.0, true).value.is_none());
    }

    #[test]
    fn zero_duration_waits_for_eligibility_then_reaches_end() {
        let mut state = state();
        state.duration = 0.0;
        assert!(sample(&state, 0.0, false).value.is_none());
        assert_eq!(sample(&state, 0.0, true).value.unwrap(), 10.0);
    }
}
