//! Compare `dyn Trait`
use std::any::Any;

pub trait AsDynCompare: Any {
    fn as_any(&self) -> &dyn Any;

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

pub trait DynCompare: AsDynCompare {
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
