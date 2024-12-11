//! Degenerate unary [`TruthFn`] that simply return its sole argument.

use super::{
    super::{Evaluable, FormulaComposer, Reducible},
    BoolFn, Connective, Formula, FunctionNotation,
};

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

impl<E: Evaluable> Reducible<1, E> for LogicalIdentity {
    fn try_reduce(&self, [value]: [E; 1]) -> Option<E> {
        Some(value)
    }
}

impl<T> FormulaComposer<1, T> for LogicalIdentity {
    fn compose(&self, [expr]: [Formula<T>; 1]) -> Formula<T> {
        expr
    }
}

impl Connective<1> for LogicalIdentity {
    fn notation(&self) -> FunctionNotation {
        FunctionNotation::Empty
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            FunctionNotation::common("identity"),
            // https://en.wikipedia.org/wiki/Buffer_gate
            FunctionNotation::scheme_gate("Buffer"),
        ])
    }
}
