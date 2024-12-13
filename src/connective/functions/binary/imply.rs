//! The _material conditional_ aka _material implication_
//! is a logical operation that formally express
//! conditional sentences in natural language.
//!
//! <https://en.wikipedia.org/wiki/Material_conditional>
use crate::formula::{Formula, Implies};

use super::super::{
    super::{Evaluable, FormulaComposer, Reducible},
    BoolFn, Connective, FunctionNotation,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Material implication is an operation on two logical values,
/// that produces a value of `true`
/// unless its first argument is `true` and its second argument is `false`.
pub struct MaterialImplication;

impl BoolFn<2> for MaterialImplication {
    fn eval(&self, [antecedent, consequent]: [bool; 2]) -> bool {
        !antecedent || consequent
    }
}

impl<E: Evaluable<Partial = T>, T> Reducible<2, E> for MaterialImplication
where
    T: std::ops::Not<Output = T>,
{
    fn try_reduce(&self, [x, y]: [E; 2]) -> Result<E, [E; 2]> {
        match (x.into_terminal(), y.into_terminal()) {
            (Err(x), Err(y)) => Err([E::partial(x), E::partial(y)]),
            (Err(antecedent), Ok(consequent)) => {
                if consequent {
                    Ok(E::tautology())
                } else {
                    Ok(E::partial(!antecedent))
                }
            }
            (Ok(antecedent), Err(consequent)) => {
                if antecedent {
                    Ok(E::partial(consequent))
                } else {
                    Ok(E::tautology())
                }
            }
            (Ok(val1), Ok(val2)) => Ok(E::terminal(self.eval([val1, val2]))),
        }
    }
}

impl<T> FormulaComposer<2, T> for MaterialImplication {
    fn compose(&self, [antecedent, consequent]: [Formula<T>; 2]) -> Formula<T> {
        antecedent.implies(consequent)
    }
}

impl Connective<2> for MaterialImplication {
    fn notation(&self) -> FunctionNotation {
        '→'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '≤'.into(),
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Basic_logic_symbols
            '⊃'.into(),
            '⇒'.into(),
            FunctionNotation::symbolic_str("->"),
            FunctionNotation::Polish('C'),
            // https://en.wikipedia.org/wiki/IMPLY_gate
            FunctionNotation::scheme_gate("IMPLY"),
        ])
    }
}
