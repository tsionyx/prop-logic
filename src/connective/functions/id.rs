//! Degenerate unary [`TruthFn`] that simply return its sole argument.

use super::{super::Evaluation, BoolFn, Connective, Formula, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// The unary
/// [identity function](https://en.wikipedia.org/wiki/Identity_function)
/// for a proposition.
pub struct LogicalIdentity;

impl BoolFn<1> for LogicalIdentity {
    fn eval(&self, [value]: [bool; 1]) -> bool {
        // the truth value of the single given proposition
        // <https://en.wikipedia.org/wiki/Truth_value>
        value
    }
}

impl TruthFn<1> for LogicalIdentity {
    fn init() -> Self {
        Self
    }

    fn reduce<T>(&self, [value]: [Evaluation<T>; 1]) -> Option<Evaluation<T>>
    where
        Self: Sized,
        T: std::ops::Not<Output = T>,
    {
        Some(value)
    }

    fn apply<T>(&self, [expr]: [Formula<T>; 1]) -> Formula<T> {
        expr
    }
}

impl Connective<1> for LogicalIdentity {
    fn notation(&self) -> FunctionNotation {
        // degenerate implementation
        "".into()
    }
}
