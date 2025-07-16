#[cfg(feature = "arbitrary")]
pub mod arbitrary;
pub mod connective;
pub mod equivalences;
mod eval;
pub mod formula;
mod ops;
pub mod truth_table;

#[derive(Debug, Copy, Clone)]
/// The order in which a transformation
/// applied recursively to a [`Formula`][formula::Formula].
///
/// Used in [`Formula::apply_rec`][formula::Formula::apply_rec].
pub enum RecursiveApplicationOrder {
    /// First apply the transormation to the top-level,
    /// then descend into the sub-formulae.
    TopDown,
    /// First apply the transformation to the sub-formulae,
    /// then apply it to the top-level.
    BottomUp,
}
