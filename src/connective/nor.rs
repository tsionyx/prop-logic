//! _Logical NOR_ or _Peirce arrow_
//! aka _Webb operator_ aka _Quine dagger_
//! is a logical operation that is the
//! [negation][super::neg] of [disjunction][super::or].
//! It is also called _non-disjunction_, or _joint denial_
//! since it says in effect that both of its operands are `false`.
//!
//! <https://en.wikipedia.org/wiki/Logical_NOR>
use crate::ops::Or;

use super::{Connective, Formula, FunctionNotation, TruthFunction};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
/// Non-disjunction is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// if and only if both of the operands are `false`.
pub struct NonDisjunction;

impl TruthFunction<2> for NonDisjunction {
    fn eval([disjunct1, disjunct2]: [bool; 2]) -> bool {
        !disjunct1 && !disjunct2
    }

    fn apply<T>([disjunct1, disjunct2]: [Formula<T>; 2]) -> Formula<T> {
        !(disjunct1.or(disjunct2))
    }
}

impl Connective for NonDisjunction {
    const ARITY: usize = 2;

    fn notation() -> FunctionNotation {
        '↓'.into()
    }

    fn alternate_notations() -> Option<Vec<FunctionNotation>> {
        Some(vec![
            "Xpq".into(), // Polish notation
            "NOR".into(),
        ])
    }
}
