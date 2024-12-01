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
    super::{Evaluation, FormulaComposer, Reducible},
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

impl<T> Reducible<2, T> for NonConjunction
where
    T: std::ops::Not<Output = T>,
{
    fn try_reduce(&self, values: [Evaluation<T>; 2]) -> Option<Evaluation<T>> {
        use Evaluation::{Partial, Terminal};
        match values {
            [Partial(_), Partial(_)] => None,
            // **Sheffer stroke** is _commutative_
            [Partial(x), Terminal(val)] | [Terminal(val), Partial(x)] => {
                if val {
                    Some(Partial(!x))
                } else {
                    Some(Evaluation::tautology())
                }
            }
            [Terminal(val1), Terminal(val2)] => Some(Terminal(self.eval([val1, val2]))),
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
