//! _Sheffer stroke_ is a logical operation that is the
//! [negation][super::neg] of [conjunction][super::and]
//!
//! expressed in ordinary language as "not both".
//! It is also called _non-conjunction_, or _alternative denial_
//! since it says in effect that at least one of its operands is `false`.
//!
//! <https://en.wikipedia.org/wiki/Sheffer_stroke>
use crate::formula::{And, Formula};

use super::super::{
    super::{Evaluable, FormulaComposer, Reducible},
    BoolFn, Connective, FunctionNotation,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Non-conjunction is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// if and only if at least one of the operands is `false`.
pub struct NonConjunction;

impl BoolFn<2> for NonConjunction {
    fn eval(&self, [conjunct1, conjunct2]: [bool; 2]) -> bool {
        !conjunct1 || !conjunct2
    }
}

impl<E: Evaluable<Partial = T>, T> Reducible<2, E> for NonConjunction
where
    T: std::ops::Not<Output = T>,
{
    fn try_reduce(&self, [x, y]: [E; 2]) -> Result<E, [E; 2]> {
        match (x.into_terminal(), y.into_terminal()) {
            (Err(x), Err(y)) => Err([E::partial(x), E::partial(y)]),
            // **Sheffer stroke** is _commutative_
            (Err(x), Ok(val)) | (Ok(val), Err(x)) => {
                if val {
                    Ok(E::partial(!x))
                } else {
                    Ok(E::tautology())
                }
            }
            (Ok(val1), Ok(val2)) => Ok(E::terminal(self.eval([val1, val2]))),
        }
    }
}

impl<T> FormulaComposer<2, T> for NonConjunction {
    fn compose(&self, [conjunct1, conjunct2]: [Formula<T>; 2]) -> Formula<T> {
        !(conjunct1.and(conjunct2))
    }
}

impl Connective<2> for NonConjunction {
    fn notation(&self) -> FunctionNotation {
        '↑'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Advanced_or_rarely_used_logical_symbols
            '|'.into(),
            '⊼'.into(),
            // short for Polish `dysjunkcja`
            FunctionNotation::Polish('D'),
            // https://en.wikipedia.org/wiki/NAND_gate
            FunctionNotation::scheme_gate("NAND"),
        ])
    }
}
