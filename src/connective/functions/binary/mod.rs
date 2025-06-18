//! Collection of all possible non-degenerate binary truth functions.
pub mod and;
pub mod converse_imply;
pub mod converse_nimply;
pub mod imply;
pub mod nand;
pub mod nimply;
pub mod nor;
pub mod or;
pub mod proj;
pub mod xnor;
pub mod xor;

impl crate::formula::Atom for char {}
