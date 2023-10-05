//! _Logical biconditional_, aka _material biconditional_
//! or _equivalence_ or _biimplication_ or _bientailment_,
//! is a logical operation that formally express
//! the notion of equality of its operands.
//!
//! <https://en.wikipedia.org/wiki/Logical_biconditional>
//! <https://en.wikipedia.org/wiki/Logical_equality>
use crate::ops::Equivalent;

use super::{Connective, Formula, FunctionNotation, TruthFunction};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
/// Logical biconditional is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// if and only if both operands are `false` or both operands are `true`.
pub struct LogicalBiconditional;

impl TruthFunction<2> for LogicalBiconditional {
    fn eval([antecedent, consequent]: [bool; 2]) -> bool {
        antecedent == consequent
    }

    fn apply<T>([antecedent, consequent]: [Formula<T>; 2]) -> Formula<T> {
        antecedent.equivalent(consequent)
    }
}

impl Connective for LogicalBiconditional {
    const ARITY: usize = 2;

    fn notation() -> FunctionNotation {
        '↔'.into()
    }

    fn alternate_notations() -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '='.into(),
            '∼'.into(),
            '⇔'.into(),
            '≡'.into(),
            "⊃⊂".into(),
            "⊂⊃".into(),
            "Epq".into(), // short for Polish `ekwiwalencja`
            "Qpq".into(), // alternate Polish notation
            "XNOR".into(),
            "IFF".into(),
            "EQ".into(),
            "EQV".into(),
        ])
    }
}
