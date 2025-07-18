//! The _material conditional_ aka _material implication_
//! is a logical operation that formally express
//! conditional sentences in natural language.
//!
//! <https://en.wikipedia.org/wiki/Material_conditional>
use crate::formula::{Implies, Not};

use super::super::{
    super::{Connective, Evaluable, FunctionNotation, TruthFn},
    neg::Negation,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Material implication is an operation on two logical values,
/// that produces a value of `true`
/// unless its first argument is `true` and its second argument is `false`.
pub struct MaterialImplication;

impl<E> TruthFn<2, E> for MaterialImplication
where
    E: Evaluable + Implies + Not,
{
    fn try_reduce(&self, [x, y]: [E; 2]) -> Result<E, [E; 2]> {
        match (x.into_terminal(), y.into_terminal()) {
            (Ok(antecedent), Ok(consequent)) => Ok(E::terminal(antecedent.implies(consequent))),
            (Ok(antecedent), Err(consequent)) => {
                if antecedent {
                    Ok(E::partial(consequent))
                } else {
                    Ok(E::tautology())
                }
            }
            (Err(antecedent), Ok(consequent)) => {
                if consequent {
                    Ok(E::tautology())
                } else {
                    Ok(Negation.compose([E::partial(antecedent)]))
                }
            }
            (Err(x), Err(y)) => Err([E::partial(x), E::partial(y)]),
        }
    }

    fn compose(&self, [antecedent, consequent]: [E; 2]) -> E {
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
