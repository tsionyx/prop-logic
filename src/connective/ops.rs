//! Unary operations on the [`TruthFunction`]-s.
use super::{functions::*, TruthFunction};

/// Easily convert a `TruthFunction` into its counterpart in terms
/// of switching all the bits in its truth table.
///
/// TODO: impl for all `TruthFunction`-s ((also as the operator `std::ops::Not`)
pub trait Negate<const ARITY: usize>: TruthFunction<ARITY> {
    /// Another `TruthFunction` which truth table is a negation of the original one.
    type Not: TruthFunction<ARITY>;
}

/// Easily convert a binary `TruthFunction` into its counterpart in terms
/// of swapping its arguments.
///
/// TODO: impl for all `TruthFunction<2>`-s
pub trait Converse: TruthFunction<2> {
    /// Another `TruthFunction` which truth function is an conversion of the original one.
    type Conversion: TruthFunction<2>;
}

impl<const ARITY: usize> Negate<ARITY> for Falsity {
    type Not = Truth;
}

impl<const ARITY: usize> Negate<ARITY> for Truth {
    type Not = Falsity;
}
