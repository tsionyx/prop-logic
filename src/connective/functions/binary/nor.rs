//! _Logical NOR_ or _Peirce arrow_
//! aka _Webb operator_ aka _Quine dagger_
//!
//! is a logical operation that is the
//! [negation][super::neg] of [disjunction][super::or].
//! It is also called _non-disjunction_, or _joint denial_
//! since it says in effect that both of its operands are `false`.
//!
//! <https://en.wikipedia.org/wiki/Logical_NOR>
use crate::formula::{Formula, Or};

use super::super::{
    super::{Evaluable, FormulaComposer, Reducible},
    BoolFn, Connective, FunctionNotation,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Non-disjunction is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// if and only if both of the operands are `false`.
pub struct NonDisjunction;

impl BoolFn<2> for NonDisjunction {
    fn eval(&self, [disjunct1, disjunct2]: [bool; 2]) -> bool {
        !disjunct1 && !disjunct2
    }
}

impl<E: Evaluable<Partial = T>, T> Reducible<2, E> for NonDisjunction
where
    T: std::ops::Not<Output = T>,
{
    fn try_reduce(&self, [x, y]: [E; 2]) -> Option<E> {
        match (x.into_terminal(), y.into_terminal()) {
            (Err(_), Err(_)) => None,
            // **Peirce arrow** is _commutative_
            (Err(x), Ok(val)) | (Ok(val), Err(x)) => {
                if val {
                    Some(E::contradiction())
                } else {
                    Some(E::partial(!x))
                }
            }
            (Ok(val1), Ok(val2)) => Some(E::terminal(self.eval([val1, val2]))),
        }
    }
}

impl<T> FormulaComposer<2, T> for NonDisjunction {
    fn compose(&self, [disjunct1, disjunct2]: [Formula<T>; 2]) -> Formula<T> {
        !(disjunct1.or(disjunct2))
    }
}

impl Connective<2> for NonDisjunction {
    fn notation(&self) -> FunctionNotation {
        '↓'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Advanced_or_rarely_used_logical_symbols
            '⊽'.into(),
            FunctionNotation::Polish('X'),
            // https://en.wikipedia.org/wiki/NOR_gate
            FunctionNotation::scheme_gate("NOR"),
        ])
    }
}
