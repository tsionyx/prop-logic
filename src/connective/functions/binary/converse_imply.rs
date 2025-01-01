//! The _converse conditional_ aka _converse implication_
//! is a logical operation that is the result of
//! reversing the [material implication][super::imply]'s
//! _antecedent_ and _consequent_.
//!
//! <https://en.wikipedia.org/wiki/Converse_implication>
use std::ops::{BitOr, Not};

use super::super::super::{Connective, Evaluable, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Converse implication is an operation on two logical values,
/// that produces a value of `true`
/// unless its first argument is `false` and its second argument is `true`.
pub struct ConverseImplication;

impl<E> TruthFn<2, E> for ConverseImplication
where
    E: Evaluable + Not<Output = E> + BitOr<Output = E>,
{
    fn fold(&self, [x, y]: [E; 2]) -> Result<E, [E; 2]> {
        match (x.into_terminal(), y.into_terminal()) {
            (Ok(consequent), Ok(antecedent)) => Ok(E::terminal(!antecedent || consequent)),
            (Ok(consequent), Err(antecedent)) => {
                if consequent {
                    Ok(E::tautology())
                } else {
                    Ok(!E::partial(antecedent))
                }
            }
            (Err(consequent), Ok(antecedent)) => {
                if antecedent {
                    Ok(E::partial(consequent))
                } else {
                    Ok(E::tautology())
                }
            }
            (Err(x), Err(y)) => Err([E::partial(x), E::partial(y)]),
        }
    }

    fn compose(&self, terms: [E; 2]) -> E {
        self.fold(terms)
            .unwrap_or_else(|[consequent, antecedent]| !antecedent | consequent)
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
