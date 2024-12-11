//! Logical disjunction is a binary operation that
//! is `true` when either or both of its operands are true.
//!
//! <https://en.wikipedia.org/wiki/Logical_disjunction>
use crate::formula::{Formula, Or};

use super::super::{
    super::{Evaluable, FormulaComposer, Reducible},
    BoolFn, Connective, FunctionNotation,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Logical disjunction is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// unless both of its arguments are `false`.
pub struct Disjunction;

impl BoolFn<2> for Disjunction {
    fn eval(&self, [disjunct1, disjunct2]: [bool; 2]) -> bool {
        disjunct1 || disjunct2
    }
}

impl<E: Evaluable> Reducible<2, E> for Disjunction {
    fn try_reduce(&self, [x, y]: [E; 2]) -> Option<E> {
        match (x.into_terminal(), y.into_terminal()) {
            (Err(_), Err(_)) => None,
            // **disjunction** is _commutative_
            (Err(x), Ok(val)) | (Ok(val), Err(x)) => {
                if val {
                    Some(E::tautology())
                } else {
                    Some(E::partial(x))
                }
            }
            (Ok(val1), Ok(val2)) => Some(E::terminal(self.eval([val1, val2]))),
        }
    }
}

impl<T> FormulaComposer<2, T> for Disjunction {
    fn compose(&self, [disjunct1, disjunct2]: [Formula<T>; 2]) -> Formula<T> {
        disjunct1.or(disjunct2)
    }
}

impl Connective<2> for Disjunction {
    fn notation(&self) -> FunctionNotation {
        '∨'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Basic_logic_symbols
            '+'.into(),
            '∥'.into(),
            FunctionNotation::symbolic_str("||"),
            FunctionNotation::common("or"),
            // https://en.wikipedia.org/wiki/OR_gate
            FunctionNotation::scheme_gate("OR"),
            // short for Polish `alternatywa`
            FunctionNotation::Polish('A'),
        ])
    }
}
