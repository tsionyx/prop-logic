//! Logical conjunction is a binary operation that
//! is `true` if and only if all of its operands are true.
//!
//! <https://en.wikipedia.org/wiki/Logical_conjunction>
use crate::formula::{And, Formula};

use super::super::{
    super::{Evaluation, FormulaComposer, Reducible},
    BoolFn, Connective, FunctionNotation, TruthFn,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Logical conjunction is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// if and only if both of its operands are `true`.
pub struct Conjunction;

impl BoolFn<2> for Conjunction {
    fn eval(&self, [conjunct1, conjunct2]: [bool; 2]) -> bool {
        conjunct1 && conjunct2
    }
}

impl TruthFn<2> for Conjunction {
    fn init() -> Self {
        Self
    }
}

impl<T> Reducible<2, T> for Conjunction {
    fn try_reduce(&self, values: [Evaluation<T>; 2]) -> Option<Evaluation<T>> {
        use Evaluation::{Partial, Terminal};
        match values {
            [Partial(_), Partial(_)] => None,
            // **conjunction** is _commutative_
            [Partial(x), Terminal(val)] | [Terminal(val), Partial(x)] => {
                if val {
                    Some(Partial(x))
                } else {
                    Some(Evaluation::contradiction())
                }
            }
            [Terminal(val1), Terminal(val2)] => Some(Terminal(self.eval([val1, val2]))),
        }
    }
}

impl<T> FormulaComposer<2, T> for Conjunction {
    fn compose(&self, [conjunct1, conjunct2]: [Formula<T>; 2]) -> Formula<T> {
        conjunct1.and(conjunct2)
    }
}

impl Connective<2> for Conjunction {
    fn notation(&self) -> FunctionNotation {
        '∧'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
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
