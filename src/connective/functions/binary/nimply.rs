//! The _material nonimplication_ aka _abjunction_
//! is a logical operation that is the
//! [negation][super::neg] of [implication][super::imply].
//!
//! <https://en.wikipedia.org/wiki/Material_nonimplication>
use std::ops::{BitAnd, Not};

use super::super::super::{Connective, Evaluable, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Material nonimplication is an operation on two logical values,
/// that produces a value of `false`
/// unless its first argument is `true` and its second argument is `false`.
pub struct MaterialNonImplication;

impl<E> TruthFn<2, E> for MaterialNonImplication
where
    E: Evaluable + Not<Output = E> + BitAnd<Output = E>,
{
    fn fold(&self, [x, y]: [E; 2]) -> Result<E, [E; 2]> {
        match (x.into_terminal(), y.into_terminal()) {
            (Ok(antecedent), Ok(consequent)) => Ok(E::terminal(antecedent && !consequent)),
            (Ok(antecedent), Err(consequent)) => {
                if antecedent {
                    Ok(!E::partial(consequent))
                } else {
                    Ok(E::contradiction())
                }
            }
            (Err(antecedent), Ok(consequent)) => {
                if consequent {
                    Ok(E::contradiction())
                } else {
                    Ok(E::partial(antecedent))
                }
            }
            (Err(x), Err(y)) => Err([E::partial(x), E::partial(y)]),
        }
    }

    fn compose(&self, terms: [E; 2]) -> E {
        self.fold(terms)
            .unwrap_or_else(|[antecedent, consequent]| antecedent & !consequent)
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
