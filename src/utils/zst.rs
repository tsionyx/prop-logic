//! Helper to ensure the type is ZST in compile-time.

use std::mem::size_of;

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
