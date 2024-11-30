pub mod dependent_array;
mod macros;
pub mod operation;
pub mod upcast;
pub mod zst;

pub(crate) use self::macros::cartesian_diag;
pub use self::zst::Zst;

pub use smol_str::SmolStr as Str;
