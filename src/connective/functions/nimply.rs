//! The _material nonimplication_ aka _abjunction_
//! is a logical operation that is the
//! [negation][super::neg] of [implication][super::imply].
//!
//! <https://en.wikipedia.org/wiki/Material_nonimplication>
use crate::formula::{Formula, Implies};

use super::{Connective, FunctionNotation, TruthFunction};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
/// Material nonimplication is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `false`
/// unless its first argument is `true` and its second argument is `false`.
pub struct MaterialNonImplication;

impl TruthFunction<2> for MaterialNonImplication {
    fn init() -> Self {
        Self
    }

    fn eval(&self, [antecedent, consequent]: [bool; 2]) -> bool {
        antecedent && !consequent
    }

    fn apply<T>(&self, [antecedent, consequent]: [Formula<T>; 2]) -> Formula<T> {
        !(antecedent.implies(consequent))
    }
}

impl Connective<2> for MaterialNonImplication {
    fn notation(&self) -> FunctionNotation {
        '↛'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '⊅'.into(),
            '>'.into(),
            "Lpq".into(), // Polish notation
            "NIMPLY".into(),
        ])
    }
}
