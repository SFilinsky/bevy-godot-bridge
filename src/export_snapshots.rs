//! Current and previous published values for visual interpolation.
#[derive(Clone, Debug, PartialEq)]
pub struct SnapshotPair<T> {
    pub current: T,
    pub previous: T,
}
