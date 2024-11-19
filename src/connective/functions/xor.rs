//! Exclusive disjunction is a binary operation that
//! is `true` if and only if its arguments differ.
//!
//! <https://en.wikipedia.org/wiki/Exclusive_or>
use crate::formula::{Formula, Xor};

use super::{super::Evaluation, BoolFn, Connective, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Exclusive disjunction is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// if and only if one is `true` and the other is `false`.
pub struct ExclusiveDisjunction;

impl BoolFn<2> for ExclusiveDisjunction {
    fn eval(&self, [disjunct1, disjunct2]: [bool; 2]) -> bool {
        disjunct1 ^ disjunct2
    }
}

impl TruthFn<2> for ExclusiveDisjunction {
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
            // **exclusive disjunction** is _commutative_
            [Partial(x), Terminal(val)] | [Terminal(val), Partial(x)] => {
                if val {
                    Some(Partial(!x))
                } else {
                    Some(Partial(x))
                }
            }
            [Terminal(val1), Terminal(val2)] => Some(Terminal(self.eval([val1, val2]))),
        }
    }

    fn apply<T>(&self, [disjunct1, disjunct2]: [Formula<T>; 2]) -> Formula<T> {
        disjunct1.xor(disjunct2)
    }
}

impl Connective<2> for ExclusiveDisjunction {
    fn notation(&self) -> FunctionNotation {
        '⊕'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
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
