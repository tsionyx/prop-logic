//! Degenerate unary [`TruthFn`] that simply return its sole argument.
use super::super::{Connective, Evaluable, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// The unary
/// [identity function](https://en.wikipedia.org/wiki/Identity_function)
/// for a proposition.
pub struct LogicalIdentity;

impl<E: Evaluable> TruthFn<1, E> for LogicalIdentity {
    fn fold(&self, [value]: [E; 1]) -> Result<E, [E; 1]> {
        Ok(value)
    }

    fn compose(&self, [value]: [E; 1]) -> E {
        // the truth value of the single given proposition
        // <https://en.wikipedia.org/wiki/Truth_value>
        value
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
