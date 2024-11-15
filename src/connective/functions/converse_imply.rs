//! The _converse conditional_ aka _converse implication_
//! is a logical operation that is the result of
//! reversing the [material implication][super::imply]'s
//! _antecedent_ and _consequent_.
//!
//! <https://en.wikipedia.org/wiki/Converse_implication>
use crate::formula::{Formula, Implies};

use super::{BoolFn, Connective, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Converse implication is an operation on two logical values,
/// that produces a value of `true`
/// unless its first argument is `false` and its second argument is `true`.
pub struct ConverseImplication;

impl BoolFn<2> for ConverseImplication {
    fn eval(&self, [consequent, antecedent]: [bool; 2]) -> bool {
        consequent || !antecedent
    }
}

impl TruthFn<2> for ConverseImplication {
    fn init() -> Self {
        Self
    }

    fn apply<T>(&self, [consequent, antecedent]: [Formula<T>; 2]) -> Formula<T> {
        antecedent.implies(consequent)
    }
}

impl Connective<2> for ConverseImplication {
    fn notation(&self) -> FunctionNotation {
        '←'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '⊂'.into(),
            '≥'.into(),
            "Bpq".into(), // Polish notation
        ])
    }
}
