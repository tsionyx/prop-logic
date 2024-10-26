//! The _material conditional_ aka _material implication_
//! is a logical operation that formally express
//! conditional sentences in natural language.
//!
//! <https://en.wikipedia.org/wiki/Material_conditional>
use crate::ops::Implies;

use super::{Connective, Formula, FunctionNotation, TruthFunction};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
/// Material implication is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// unless its first argument is `true` and its second argument is `false`.
pub struct MaterialImplication;

impl TruthFunction<2> for MaterialImplication {
    fn init() -> Self {
        Self
    }

    fn eval(&self, [antecedent, consequent]: [bool; 2]) -> bool {
        !antecedent || consequent
    }

    fn apply<T>(&self, [antecedent, consequent]: [Formula<T>; 2]) -> Formula<T> {
        antecedent.implies(consequent)
    }
}

impl Connective<2> for MaterialImplication {
    fn notation(&self) -> FunctionNotation {
        '→'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '⊃'.into(),
            '⇒'.into(),
            '≤'.into(),
            "Cpq".into(), // Polish notation
            "IMPLY".into(),
        ])
    }
}