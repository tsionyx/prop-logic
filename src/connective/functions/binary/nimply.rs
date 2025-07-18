//! The _material nonimplication_ aka _abjunction_
//! is a logical operation that is the
//! [negation][super::neg] of [implication][super::imply].
//!
//! <https://en.wikipedia.org/wiki/Material_nonimplication>
use crate::formula::{Implies, Not};

use super::{
    super::{
        super::{Connective, Evaluable, FunctionNotation, TruthFn},
        neg::Negation,
    },
    imply::MaterialImplication,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Material nonimplication is an operation on two logical values,
/// that produces a value of `false`
/// unless its first argument is `true` and its second argument is `false`.
pub struct MaterialNonImplication;

impl<E> TruthFn<2, E> for MaterialNonImplication
where
    E: Evaluable + Implies + Not,
{
    fn try_reduce(&self, terms: [E; 2]) -> Result<E, [E; 2]> {
        MaterialImplication.try_reduce(terms).map(Negation::negate)
    }

    fn compose(&self, terms: [E; 2]) -> E {
        Negation.compose([MaterialImplication.compose(terms)])
    }
}

impl Connective<2> for MaterialNonImplication {
    fn notation(&self) -> FunctionNotation {
        '↛'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '>'.into(),
            '⊅'.into(),
            // https://en.wikipedia.org/wiki/Arrows_(Unicode_block)
            '⇏'.into(),
            FunctionNotation::Polish('L'),
            // https://en.wikipedia.org/wiki/NIMPLY_gate
            FunctionNotation::scheme_gate("NIMPLY"),
        ])
    }
}
