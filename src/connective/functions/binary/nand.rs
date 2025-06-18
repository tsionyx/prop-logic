//! _Sheffer stroke_ is a logical operation that is the
//! [negation][super::neg] of [conjunction][super::and]
//!
//! expressed in ordinary language as "not both".
//! It is also called _non-conjunction_, or _alternative denial_
//! since it says in effect that at least one of its operands is `false`.
//!
//! <https://en.wikipedia.org/wiki/Sheffer_stroke>
use crate::formula::{And, Not};

use super::{
    super::{
        super::{Connective, Evaluable, FunctionNotation, TruthFn},
        neg::Negation,
    },
    and::Conjunction,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Non-conjunction is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// if and only if at least one of the operands is `false`.
pub struct NonConjunction;

impl<E> TruthFn<2, E> for NonConjunction
where
    E: Evaluable + And + Not,
{
    fn fold(&self, terms: [E; 2]) -> Result<E, [E; 2]> {
        Conjunction.fold(terms).map(Negation::negate)
    }

    fn compose(&self, terms: [E; 2]) -> E {
        Negation.compose([Conjunction.compose(terms)])
    }
}

impl Connective<2> for NonConjunction {
    fn notation(&self) -> FunctionNotation {
        '↑'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Advanced_or_rarely_used_logical_symbols
            '|'.into(),
            '⊼'.into(),
            // short for Polish `dysjunkcja`
            FunctionNotation::Polish('D'),
            // https://en.wikipedia.org/wiki/NAND_gate
            FunctionNotation::scheme_gate("NAND"),
        ])
    }
}
