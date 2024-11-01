pub(crate) mod dependent_array;
mod macros;
pub(crate) mod upcast;
mod zst;

pub(crate) use self::macros::cartesian_diag;
pub use self::zst::Zst;
