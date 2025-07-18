//! The _converse conditional_ aka _converse implication_
//! is a logical operation that is the result of
//! reversing the [material implication][super::imply]'s
//! _antecedent_ and _consequent_.
//!
//! <https://en.wikipedia.org/wiki/Converse_implication>
use crate::formula::{Implies, Not};

use super::{
    super::super::{Connective, Evaluable, FunctionNotation, TruthFn},
    imply::MaterialImplication,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Converse implication is an operation on two logical values,
/// that produces a value of `true`
/// unless its first argument is `false` and its second argument is `true`.
pub struct ConverseImplication;

impl<E> TruthFn<2, E> for ConverseImplication
where
    E: Evaluable + Implies + Not,
{
    fn try_reduce(&self, [x, y]: [E; 2]) -> Result<E, [E; 2]> {
        MaterialImplication
            .try_reduce([y, x])
            .map_err(|[y, x]| [x, y])
    }

    fn compose(&self, [x, y]: [E; 2]) -> E {
        MaterialImplication.compose([y, x])
    }
}

impl Connective<2> for ConverseImplication {
    fn notation(&self) -> FunctionNotation {
        '←'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '≥'.into(),
            '⊂'.into(),
            // https://en.wikipedia.org/wiki/Arrows_(Unicode_block)
            '⇐'.into(),
            FunctionNotation::symbolic_str("<-"),
            FunctionNotation::Polish('B'),
        ])
    }
}
