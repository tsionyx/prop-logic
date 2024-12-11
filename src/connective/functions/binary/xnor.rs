//! _Logical biconditional_, aka _material biconditional_
//! or _equivalence_ or _biimplication_ or _bientailment_,
//! is a logical operation that formally express
//! the notion of equality of its operands.
//!
//! <https://en.wikipedia.org/wiki/Logical_biconditional>
//! <https://en.wikipedia.org/wiki/Logical_equality>
use crate::formula::{Equivalent, Formula};

use super::super::{
    super::{Evaluable, FormulaComposer, Reducible},
    BoolFn, Connective, FunctionNotation,
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

impl<E: Evaluable<Partial = T>, T> Reducible<2, E> for LogicalBiconditional
where
    T: std::ops::Not<Output = T>,
{
    fn try_reduce(&self, [x, y]: [E; 2]) -> Option<E> {
        match (x.into_terminal(), y.into_terminal()) {
            (Err(_), Err(_)) => None,
            // **equivalence** is _commutative_
            (Err(x), Ok(val)) | (Ok(val), Err(x)) => {
                if val {
                    Some(E::partial(x))
                } else {
                    Some(E::partial(!x))
                }
            }
            (Ok(val1), Ok(val2)) => Some(E::terminal(self.eval([val1, val2]))),
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
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Basic_logic_symbols
            '⇔'.into(),
            '≡'.into(),
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Advanced_or_rarely_used_logical_symbols
            '⊙'.into(),
            FunctionNotation::symbolic_str("⊃⊂"),
            FunctionNotation::symbolic_str("⊂⊃"),
            FunctionNotation::common("iff"),
            FunctionNotation::common("eq"),
            // https://en.wikipedia.org/wiki/XOR_gate
            FunctionNotation::scheme_gate("XNOR"),
            // short for Polish `ekwiwalencja` (1929)
            FunctionNotation::Polish('E'),
            // alternate Polish notation (1951)
            FunctionNotation::Polish('Q'),
        ])
    }
}
