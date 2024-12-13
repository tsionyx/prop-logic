//! Negation aka logical NOT aka logical complement,
//! is an unary operation that reverse its only argument.
//!
//! <https://en.wikipedia.org/wiki/Negation>
use super::{
    super::{Evaluable, FormulaComposer, Reducible},
    BoolFn, Connective, Formula, FunctionNotation,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Unary operation that takes a proposition P
/// to another proposition "not P"
/// with [its value](https://en.wikipedia.org/wiki/Truth_value)
/// switched.
pub struct Negation;

impl BoolFn<1> for Negation {
    fn eval(&self, [value]: [bool; 1]) -> bool {
        !value
    }
}

impl<E, T> Reducible<1, E> for Negation
where
    E: Evaluable<Partial = T> + std::ops::Not<Output = E>,
{
    fn try_reduce(&self, [value]: [E; 1]) -> Result<E, [E; 1]> {
        Ok(!value)
    }
}

impl<T> FormulaComposer<1, T> for Negation {
    fn compose(&self, [expr]: [Formula<T>; 1]) -> Formula<T> {
        !expr
    }
}

impl Connective<1> for Negation {
    fn notation(&self) -> FunctionNotation {
        '¬'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Basic_logic_symbols
            '~'.into(),
            '−'.into(),
            '!'.into(),
            FunctionNotation::common("not"),
            // https://en.wikipedia.org/wiki/NOT_gate
            FunctionNotation::scheme_gate("NOT"),
            // short for Polish `negacja`
            FunctionNotation::Polish('N'),
        ])
    }
}
