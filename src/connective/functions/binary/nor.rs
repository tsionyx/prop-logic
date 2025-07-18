//! _Logical NOR_ or _Peirce arrow_
//! aka _Webb operator_ aka _Quine dagger_
//!
//! is a logical operation that is the
//! [negation][super::neg] of [disjunction][super::or].
//! It is also called _non-disjunction_, or _joint denial_
//! since it says in effect that both of its operands are `false`.
//!
//! <https://en.wikipedia.org/wiki/Logical_NOR>
use crate::formula::{Not, Or};

use super::{
    super::{
        super::{Connective, Evaluable, FunctionNotation, TruthFn},
        neg::Negation,
    },
    or::Disjunction,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Non-disjunction is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// if and only if both of the operands are `false`.
pub struct NonDisjunction;

impl<E> TruthFn<2, E> for NonDisjunction
where
    E: Evaluable + Or + Not,
{
    fn try_reduce(&self, terms: [E; 2]) -> Result<E, [E; 2]> {
        Disjunction.try_reduce(terms).map(Negation::negate)
    }

    fn compose(&self, terms: [E; 2]) -> E {
        Negation.compose([Disjunction.compose(terms)])
    }
}

impl Connective<2> for NonDisjunction {
    fn notation(&self) -> FunctionNotation {
        '↓'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Advanced_or_rarely_used_logical_symbols
            '⊽'.into(),
            FunctionNotation::Polish('X'),
            // https://en.wikipedia.org/wiki/NOR_gate
            FunctionNotation::scheme_gate("NOR"),
        ])
    }
}
