pub(crate) mod dependent_array;
mod macros;
mod zst;

pub(crate) use self::macros::cartesian_diag;
pub use self::zst::Zst;
