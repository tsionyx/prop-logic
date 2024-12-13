//! Exclusive disjunction is a binary operation that
//! is `true` if and only if its arguments differ.
//!
//! <https://en.wikipedia.org/wiki/Exclusive_or>
use crate::formula::{Formula, Xor};

use super::super::{
    super::{Evaluable, FormulaComposer, Reducible},
    BoolFn, Connective, FunctionNotation,
};

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

impl<E: Evaluable<Partial = T>, T> Reducible<2, E> for ExclusiveDisjunction
where
    T: std::ops::Not<Output = T>,
{
    fn try_reduce(&self, [x, y]: [E; 2]) -> Result<E, [E; 2]> {
        match (x.into_terminal(), y.into_terminal()) {
            (Err(x), Err(y)) => Err([E::partial(x), E::partial(y)]),
            // **exclusive disjunction** is _commutative_
            (Err(x), Ok(val)) | (Ok(val), Err(x)) => {
                if val {
                    Ok(E::partial(!x))
                } else {
                    Ok(E::partial(x))
                }
            }
            (Ok(val1), Ok(val2)) => Ok(E::terminal(self.eval([val1, val2]))),
        }
    }
}

impl<T> FormulaComposer<2, T> for ExclusiveDisjunction {
    fn compose(&self, [disjunct1, disjunct2]: [Formula<T>; 2]) -> Formula<T> {
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
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Basic_logic_symbols
            '⊻'.into(),
            '↮'.into(),
            '≢'.into(),
            FunctionNotation::common("xor"),
            // https://en.wikipedia.org/wiki/XOR_gate
            FunctionNotation::scheme_gate("XOR"),
            FunctionNotation::Polish('J'),
        ])
    }
}
