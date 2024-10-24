//! Unary operations on the [`TruthFunction`]-s.
use super::TruthFunction;

/// Easily convert a `TruthFunction` into its counterpart in terms
/// of switching all the bits in its truth table.
///
/// TODO: impl for all `TruthFunction`-s ((also as the operator `std::ops::Not`)
pub trait Negate<const ARITY: usize>: TruthFunction<ARITY> {
    /// Another `TruthFunction` which truth function is an negation of the original one.
    type Not: TruthFunction<ARITY>;
}

/// Easily convert a `TruthFunction<2>` into its counterpart in terms
/// of swapping its arguments.
///
/// TODO: impl for all `TruthFunction`-s
pub trait Converse: TruthFunction<2> {
    /// Another `TruthFunction` which truth function is an conversion of the original one.
    type Inversion: TruthFunction<2>;
}
