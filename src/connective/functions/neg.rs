//! Negation aka logical NOT aka logical complement,
//! is an unary operation that reverse its only argument.
//!
//! <https://en.wikipedia.org/wiki/Negation>
use crate::formula::Not;

use super::super::{Connective, Evaluable, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Unary operation that takes a proposition P
/// to another proposition "not P"
/// with [its value](https://en.wikipedia.org/wiki/Truth_value)
/// switched.
pub struct Negation;

impl<E> TruthFn<1, E> for Negation
where
    E: Evaluable + Not,
{
    fn try_reduce(&self, [e]: [E; 1]) -> Result<E, [E; 1]> {
        e.into_terminal()
            .map(|value| E::terminal(value.not()))
            .map_err(|partial| [E::partial(partial)])
    }

    fn compose(&self, [x]: [E; 1]) -> E {
        x.not()
    }
}

impl Negation {
    pub(crate) fn negate<E>(e: E) -> E
    where
        E: Evaluable + Not,
    {
        Self.eval([e])
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
