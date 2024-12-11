//! The _material nonimplication_ aka _abjunction_
//! is a logical operation that is the
//! [negation][super::neg] of [implication][super::imply].
//!
//! <https://en.wikipedia.org/wiki/Material_nonimplication>
use crate::formula::{Formula, Implies};

use super::super::{
    super::{Evaluable, FormulaComposer, Reducible},
    BoolFn, Connective, FunctionNotation,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Material nonimplication is an operation on two logical values,
/// that produces a value of `false`
/// unless its first argument is `true` and its second argument is `false`.
pub struct MaterialNonImplication;

impl BoolFn<2> for MaterialNonImplication {
    fn eval(&self, [antecedent, consequent]: [bool; 2]) -> bool {
        antecedent && !consequent
    }
}

impl<E: Evaluable<Partial = T>, T> Reducible<2, E> for MaterialNonImplication
where
    T: std::ops::Not<Output = T>,
{
    fn try_reduce(&self, [x, y]: [E; 2]) -> Option<E> {
        match (x.into_terminal(), y.into_terminal()) {
            (Err(_), Err(_)) => None,
            (Err(antecedent), Ok(consequent)) => {
                if consequent {
                    Some(E::contradiction())
                } else {
                    Some(E::partial(antecedent))
                }
            }
            (Ok(antecedent), Err(consequent)) => {
                if antecedent {
                    Some(E::partial(!consequent))
                } else {
                    Some(E::contradiction())
                }
            }
            (Ok(val1), Ok(val2)) => Some(E::terminal(self.eval([val1, val2]))),
        }
    }
}

impl<T> FormulaComposer<2, T> for MaterialNonImplication {
    fn compose(&self, [antecedent, consequent]: [Formula<T>; 2]) -> Formula<T> {
        !(antecedent.implies(consequent))
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
