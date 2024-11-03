//! Logical disjunction is a binary operation that
//! is `true` when either or both of its operands are true.
//!
//! <https://en.wikipedia.org/wiki/Logical_disjunction>
use crate::formula::{Formula, Or};

use super::{Connective, FunctionNotation, TruthFunction};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
/// Logical disjunction is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// unless both of its arguments are `false`.
pub struct Disjunction;

impl TruthFunction<2> for Disjunction {
    fn init() -> Self {
        Self
    }

    fn eval(&self, [disjunct1, disjunct2]: [bool; 2]) -> bool {
        disjunct1 || disjunct2
    }

    fn apply<T>(&self, [disjunct1, disjunct2]: [Formula<T>; 2]) -> Formula<T> {
        disjunct1.or(disjunct2)
    }
}

impl Connective<2> for Disjunction {
    fn notation(&self) -> FunctionNotation {
        '∨'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '+'.into(),
            "||".into(),
            "Apq".into(), // short for Polish `alternatywa`
            "OR".into(),
            "or".into(),
        ])
    }
}
