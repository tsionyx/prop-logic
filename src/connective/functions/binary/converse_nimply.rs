//! The _converse nonimplication_
//! is a logical operation that is the
//! [negation][super::neg] of [converse implication][super::converse_imply]
//!
//! (equivalently, the [negation][super::neg]
//! of the converse of [implication][super::imply]).
//!
//! <https://en.wikipedia.org/wiki/Converse_nonimplication>
use std::ops::{BitOr, Not};

use super::{
    super::super::{Connective, Evaluable, FunctionNotation, TruthFn},
    nimply::MaterialNonImplication,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Converse nonimplication is an operation on two logical values,
/// that produces a value of `false`
/// unless its first argument is `false` and its second argument is `true`.
pub struct ConverseNonImplication;

impl<E> TruthFn<2, E> for ConverseNonImplication
where
    E: Evaluable + Not<Output = E> + BitOr<Output = E>,
{
    fn fold(&self, [x, y]: [E; 2]) -> Result<E, [E; 2]> {
        MaterialNonImplication.fold([y, x])
    }

    fn compose(&self, [x, y]: [E; 2]) -> E {
        MaterialNonImplication.compose([y, x])
    }
}

impl Connective<2> for ConverseNonImplication {
    fn notation(&self) -> FunctionNotation {
        '↚'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '<'.into(),
            '⊄'.into(),
            // https://en.wikipedia.org/wiki/Arrows_(Unicode_block)
            '⇍'.into(),
            FunctionNotation::Polish('M'),
        ])
    }
}
