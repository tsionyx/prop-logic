//! _Sheffer stroke_ is a logical operation that is the
//! [negation][super::neg] of [conjunction][super::and]
//! expressed in ordinary language as "not both".
//! It is also called _non-conjunction_, or _alternative denial_
//! since it says in effect that at least one of its operands is `false`.
//!
//! <https://en.wikipedia.org/wiki/Sheffer_stroke>
use crate::ops::And;

use super::{Connective, Formula, FunctionNotation, TruthFunction};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
/// Non-conjunction is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// if and only if at least one of the operands is `false`.
pub struct NonConjunction;

impl TruthFunction<2> for NonConjunction {
    fn init() -> Self {
        Self
    }

    fn eval(&self, [conjunct1, conjunct2]: [bool; 2]) -> bool {
        !conjunct1 || !conjunct2
    }

    fn apply<T>(&self, [conjunct1, conjunct2]: [Formula<T>; 2]) -> Formula<T> {
        !(conjunct1.and(conjunct2))
    }
}

impl Connective<2> for NonConjunction {
    fn notation(&self) -> FunctionNotation {
        '↑'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '|'.into(),
            "Dpq".into(), // short for Polish `dysjunkcja`
            "NAND".into(),
        ])
    }
}
