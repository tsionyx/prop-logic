//! The _converse nonimplication_
//! is a logical operation that is the
//! [negation][super::neg] of [converse implication][super::converse_imply]
//!
//! (equivalently, the [negation][super::neg]
//! of the converse of [implication][super::imply]).
//!
//! <https://en.wikipedia.org/wiki/Converse_nonimplication>
use std::ops::{BitAnd, Not};

use super::super::super::{Connective, Evaluable, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Converse nonimplication is an operation on two logical values,
/// that produces a value of `false`
/// unless its first argument is `false` and its second argument is `true`.
pub struct ConverseNonImplication;

impl<E> TruthFn<2, E> for ConverseNonImplication
where
    E: Evaluable + Not<Output = E> + BitAnd<Output = E>,
{
    fn fold(&self, [x, y]: [E; 2]) -> Result<E, [E; 2]> {
        match (x.into_terminal(), y.into_terminal()) {
            (Ok(consequent), Ok(antecedent)) => Ok(E::terminal(antecedent && !consequent)),
            (Ok(consequent), Err(antecedent)) => {
                if consequent {
                    Ok(E::contradiction())
                } else {
                    Ok(E::partial(antecedent))
                }
            }
            (Err(consequent), Ok(antecedent)) => {
                if antecedent {
                    Ok(!E::partial(consequent))
                } else {
                    Ok(E::contradiction())
                }
            }
            (Err(x), Err(y)) => Err([E::partial(x), E::partial(y)]),
        }
    }

    fn compose(&self, terms: [E; 2]) -> E {
        self.fold(terms)
            .unwrap_or_else(|[consequent, antecedent]| antecedent & !consequent)
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
