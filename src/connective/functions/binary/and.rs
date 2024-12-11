//! Logical conjunction is a binary operation that
//! is `true` if and only if all of its operands are true.
//!
//! <https://en.wikipedia.org/wiki/Logical_conjunction>
use crate::formula::{And, Formula};

use super::super::{
    super::{Evaluable, FormulaComposer, Reducible},
    BoolFn, Connective, FunctionNotation,
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

impl<E: Evaluable> Reducible<2, E> for Conjunction {
    fn try_reduce(&self, [x, y]: [E; 2]) -> Option<E> {
        match (x.into_terminal(), y.into_terminal()) {
            (Err(_), Err(_)) => None,
            // **conjunction** is _commutative_
            (Err(x), Ok(val)) | (Ok(val), Err(x)) => {
                if val {
                    Some(E::partial(x))
                } else {
                    Some(E::contradiction())
                }
            }
            (Ok(val1), Ok(val2)) => Some(E::terminal(self.eval([val1, val2]))),
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
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Basic_logic_symbols
            '&'.into(),
            '×'.into(),
            '·'.into(),
            FunctionNotation::symbolic_str("&&"),
            FunctionNotation::common("and"),
            // https://en.wikipedia.org/wiki/AND_gate
            FunctionNotation::scheme_gate("AND"),
            // short for Polish `koniunkcja`
            FunctionNotation::Polish('K'),
        ])
    }
}
