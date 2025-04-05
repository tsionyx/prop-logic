//! Compare `dyn Trait`
//!
//! <https://quinedot.github.io/rust-learning/dyn-trait-eq.html>
use std::any::Any;

/// A helper trait for coercing into `&Self` into `&dyn Any` and `&dyn DynCompare`.
pub trait AsDynCompare: Any {
    /// Helper to use [`dyn Any::downcast_ref`]
    /// into [`DynCompare::dyn_eq`] method.
    fn as_any(&self) -> &dyn Any;

    /// Helper to coerce `&Self` into [`dyn DynCompare`]
    /// to use its `PartialEq` implementation later in client code.
    fn as_dyn_compare(&self) -> &dyn DynCompare;
}

// Sized types only
impl<T: Any + DynCompare> AsDynCompare for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_dyn_compare(&self) -> &dyn DynCompare {
        self
    }
}

/// Supertrait for a trait wishing to implement `PartialEq` for `dyn Trait`.
/// This is a workaround for the fact that `PartialEq` cannot be implemented
/// for `dyn Trait` directly.
///
/// Based on the article [about dyn trait eq](https://quinedot.github.io/rust-learning/dyn-trait-eq.html).
pub trait DynCompare: AsDynCompare {
    /// Blanket method implementing comparison
    /// using [`dyn Any::downcast_ref`].
    fn dyn_eq(&self, other: &dyn DynCompare) -> bool;
}

impl<T: Any + PartialEq> DynCompare for T {
    fn dyn_eq(&self, other: &dyn DynCompare) -> bool {
        other.as_any().downcast_ref::<Self>() == Some(self)
    }
}

// n.b. this could be implemented in a more general way when
// the trait object lifetime is not constrained to `'static`
impl PartialEq<Self> for dyn DynCompare + '_ {
    fn eq(&self, other: &Self) -> bool {
        self.dyn_eq(other)
    }
}
