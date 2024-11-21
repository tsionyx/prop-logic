//! _Logical biconditional_, aka _material biconditional_
//! or _equivalence_ or _biimplication_ or _bientailment_,
//! is a logical operation that formally express
//! the notion of equality of its operands.
//!
//! <https://en.wikipedia.org/wiki/Logical_biconditional>
//! <https://en.wikipedia.org/wiki/Logical_equality>
use crate::formula::{Equivalent, Formula};

use super::{
    super::{Evaluation, FormulaComposer, Reducible},
    BoolFn, Connective, FunctionNotation, TruthFn,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Logical biconditional is an operation on two logical values,
/// that produces a value of `true`
/// if and only if both operands are `false` or both operands are `true`.
pub struct LogicalBiconditional;

impl BoolFn<2> for LogicalBiconditional {
    fn eval(&self, [antecedent, consequent]: [bool; 2]) -> bool {
        antecedent == consequent
    }
}

impl TruthFn<2> for LogicalBiconditional {
    fn init() -> Self {
        Self
    }
}

impl<T> Reducible<2, T> for LogicalBiconditional
where
    T: std::ops::Not<Output = T>,
{
    fn try_reduce(&self, values: [Evaluation<T>; 2]) -> Option<Evaluation<T>> {
        use Evaluation::{Partial, Terminal};
        match values {
            [Partial(_), Partial(_)] => None,
            // **equivalence** is _commutative_
            [Partial(x), Terminal(val)] | [Terminal(val), Partial(x)] => {
                if val {
                    Some(Partial(x))
                } else {
                    Some(Partial(!x))
                }
            }
            [Terminal(val1), Terminal(val2)] => Some(Terminal(self.eval([val1, val2]))),
        }
    }
}

impl<T> FormulaComposer<2, T> for LogicalBiconditional {
    fn compose(&self, [antecedent, consequent]: [Formula<T>; 2]) -> Formula<T> {
        antecedent.equivalent(consequent)
    }
}

impl Connective<2> for LogicalBiconditional {
    fn notation(&self) -> FunctionNotation {
        '↔'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
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
