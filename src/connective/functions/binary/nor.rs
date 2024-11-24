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
    super::{Evaluation, FormulaComposer, Reducible},
    BoolFn, Connective, FunctionNotation, TruthFn,
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

impl TruthFn<2> for NonDisjunction {
    fn init() -> Self {
        Self
    }
}

impl<T> Reducible<2, T> for NonDisjunction
where
    T: std::ops::Not<Output = T>,
{
    fn try_reduce(&self, values: [Evaluation<T>; 2]) -> Option<Evaluation<T>> {
        use Evaluation::{Partial, Terminal};
        match values {
            [Partial(_), Partial(_)] => None,
            // **Peirce arrow** is _commutative_
            [Partial(x), Terminal(val)] | [Terminal(val), Partial(x)] => {
                if val {
                    Some(Evaluation::contradiction())
                } else {
                    Some(Partial(!x))
                }
            }
            [Terminal(val1), Terminal(val2)] => Some(Terminal(self.eval([val1, val2]))),
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
            "Xpq".into(), // Polish notation
            "NOR".into(),
        ])
    }
}
