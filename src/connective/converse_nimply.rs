//! The _converse nonimplication_
//! is a logical operation that is the
//! [negation][super::neg] of [converse implication][super::converse_imply]
//! (equivalently, the [negation][super::neg]
//! of the converse of [implication][super::imply]).
//!
//! <https://en.wikipedia.org/wiki/Converse_nonimplication>
use crate::ops::Implies;

use super::{Connective, Formula, FunctionNotation, TruthFunction};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
/// Converse nonimplication is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `false`
/// unless its first argument is `false` and its second argument is `true`.
pub struct ConverseNonImplication;

impl TruthFunction<2> for ConverseNonImplication {
    fn init() -> Self {
        Self
    }

    fn eval(&self, [consequent, antecedent]: [bool; 2]) -> bool {
        !consequent && antecedent
    }

    fn apply<T>(&self, [consequent, antecedent]: [Formula<T>; 2]) -> Formula<T> {
        !(antecedent.implies(consequent))
    }
}

impl Connective<2> for ConverseNonImplication {
    fn notation(&self) -> FunctionNotation {
        '↚'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '⊄'.into(),
            '<'.into(),
            "Mpq".into(), // Polish notation
        ])
    }
}

#[cfg(test)]
mod tests {
    use super::{super::tests::apply_and_eval_is_equivalent, *};

    #[test]
    fn eval_is_sync_with_apply() {
        apply_and_eval_is_equivalent::<ConverseNonImplication, 2>();
    }
}
