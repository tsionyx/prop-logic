//! The _converse nonimplication_
//! is a logical operation that is the
//! [negation][super::neg] of [converse implication][super::converse_imply]
//!
//! (equivalently, the [negation][super::neg]
//! of the converse of [implication][super::imply]).
//!
//! <https://en.wikipedia.org/wiki/Converse_nonimplication>
use crate::formula::{Formula, Implies};

use super::{super::Evaluation, BoolFn, Connective, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Converse nonimplication is an operation on two logical values,
/// that produces a value of `false`
/// unless its first argument is `false` and its second argument is `true`.
pub struct ConverseNonImplication;

impl BoolFn<2> for ConverseNonImplication {
    fn eval(&self, [consequent, antecedent]: [bool; 2]) -> bool {
        !consequent && antecedent
    }
}

impl TruthFn<2> for ConverseNonImplication {
    fn init() -> Self {
        Self
    }

    fn reduce<T>(&self, values: [Evaluation<T>; 2]) -> Option<Evaluation<T>>
    where
        Self: Sized,
        T: std::ops::Not<Output = T>,
    {
        use Evaluation::{Partial, Terminal};
        match values {
            [Partial(_), Partial(_)] => None,
            [Partial(consequent), Terminal(antecedent)] => {
                if antecedent {
                    Some(Partial(!consequent))
                } else {
                    Some(Evaluation::contradiction())
                }
            }
            [Terminal(consequent), Partial(antecedent)] => {
                if consequent {
                    Some(Evaluation::contradiction())
                } else {
                    Some(Partial(antecedent))
                }
            }
            [Terminal(val1), Terminal(val2)] => Some(Terminal(self.eval([val1, val2]))),
        }
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
