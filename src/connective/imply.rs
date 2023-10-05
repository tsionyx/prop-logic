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
    fn eval([antecedent, consequent]: [bool; 2]) -> bool {
        !antecedent || consequent
    }

    fn apply<T>([antecedent, consequent]: [Formula<T>; 2]) -> Formula<T> {
        antecedent.implies(consequent)
    }
}

impl Connective for MaterialImplication {
    const ARITY: usize = 2;

    fn notation() -> FunctionNotation {
        '→'.into()
    }

    fn alternate_notations() -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '⊃'.into(),
            '⇒'.into(),
            '≤'.into(),
            "Cpq".into(), // Polish notation
            "IMPLY".into(),
        ])
    }
}
