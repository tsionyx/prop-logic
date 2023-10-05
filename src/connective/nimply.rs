//! The _material nonimplication_ aka _abjunction_
//! is a logical operation that is the
//! [negation][super::neg] of [implication][super::imply].
//!
//! <https://en.wikipedia.org/wiki/Material_nonimplication>
use crate::ops::Implies;

use super::{Connective, Formula, FunctionNotation, TruthFunction};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
/// Material nonimplication is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `false`
/// unless its first argument is `true` and its second argument is `false`.
pub struct MaterialNonImplication;

impl TruthFunction<2> for MaterialNonImplication {
    fn eval([antecedent, consequent]: [bool; 2]) -> bool {
        antecedent && !consequent
    }

    fn apply<T>([antecedent, consequent]: [Formula<T>; 2]) -> Formula<T> {
        !(antecedent.implies(consequent))
    }
}

impl Connective for MaterialNonImplication {
    const ARITY: usize = 2;

    fn notation() -> FunctionNotation {
        '↛'.into()
    }

    fn alternate_notations() -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '⊅'.into(),
            '>'.into(),
            "Lpq".into(), // Polish notation
            "NIMPLY".into(),
        ])
    }
}
