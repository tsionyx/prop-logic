//! Exclusive disjunction is a binary operation that
//! is `true` if and only if its arguments differ.
//!
//! <https://en.wikipedia.org/wiki/Exclusive_or>
use crate::ops::Xor;

use super::{Connective, Formula, FunctionNotation, TruthFunction};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
/// Exclusive disjunction is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// if and only if one is `true` and the other is `false`.
pub struct ExclusiveDisjunction;

impl TruthFunction<2> for ExclusiveDisjunction {
    fn eval([disjunct1, disjunct2]: [bool; 2]) -> bool {
        disjunct1 ^ disjunct2
    }

    fn apply<T>([disjunct1, disjunct2]: [Formula<T>; 2]) -> Formula<T> {
        disjunct1.xor(disjunct2)
    }
}

impl Connective for ExclusiveDisjunction {
    const ARITY: usize = 2;

    fn notation() -> FunctionNotation {
        '⊕'.into()
    }

    fn alternate_notations() -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '^'.into(),
            '⩛'.into(),
            '⊻'.into(),
            '↮'.into(),
            '≢'.into(),
            "Jpq".into(), // Polish notation
            "XOR".into(),
            "xor".into(),
            "EOR".into(),
            "EXOR".into(),
        ])
    }
}
