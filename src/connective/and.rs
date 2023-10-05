//! Logical conjunction is a binary operation that
//! is `true` if and only if all of its operands are true.
//!
//! <https://en.wikipedia.org/wiki/Logical_conjunction>
use crate::ops::And;

use super::{Connective, Formula, FunctionNotation, TruthFunction};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
/// Logical conjunction is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// if and only if both of its operands are `true`.
pub struct Conjunction;

impl TruthFunction<2> for Conjunction {
    fn eval([conjunct1, conjunct2]: [bool; 2]) -> bool {
        conjunct1 && conjunct2
    }

    fn apply<T>([conjunct1, conjunct2]: [Formula<T>; 2]) -> Formula<T> {
        conjunct1.and(conjunct2)
    }
}

impl Connective for Conjunction {
    const ARITY: usize = 2;

    fn notation() -> FunctionNotation {
        '∧'.into()
    }

    fn alternate_notations() -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '&'.into(),
            '×'.into(),
            '·'.into(),
            "&&".into(),
            "Kpq".into(), // short for Polish `koniunkcja`
            "AND".into(),
            "and".into(),
        ])
    }
}
