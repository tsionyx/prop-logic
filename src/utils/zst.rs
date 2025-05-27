//! Helper to ensure the type is ZST in compile-time.

use std::{fmt, hash, mem::size_of};

/// The wrapper for uninhabited type.
///
/// This is a wrapper ensuring `Eq`, `Hash` and `Ord` implementations.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Void(void::Void);

impl Eq for Void {}

#[expect(clippy::derive_ord_xor_partial_ord)]
impl Ord for Void {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        void::unreachable(self.0)
    }
}

impl hash::Hash for Void {
    fn hash<H: hash::Hasher>(&self, _state: &mut H) {
        void::unreachable(self.0)
    }
}

impl fmt::Display for Void {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        void::unreachable(self.0)
    }
}

/// Marker trait that will only be implemented for ZSTs
pub trait Zst {
    /// Be aware that the associated constants
    /// gets evaluated after monomorphization,
    /// so you have to explicitly call for it for every type
    /// you are asserting to be ZST.
    const ASSERT_ZST: ();
}

impl<T: Sized> Zst for T {
    const ASSERT_ZST: () = {
        let size = size_of::<T>();
        assert!(size == 0, "Type must be zero-sized");
    };
}
