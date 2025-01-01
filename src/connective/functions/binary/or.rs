//! Logical disjunction is a binary operation that
//! is `true` when either or both of its operands are true.
//!
//! <https://en.wikipedia.org/wiki/Logical_disjunction>
use std::ops::BitOr;

use super::super::super::{Connective, Evaluable, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Logical disjunction is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// unless both of its arguments are `false`.
pub struct Disjunction;

impl<E> TruthFn<2, E> for Disjunction
where
    E: Evaluable + BitOr<Output = E>,
{
    fn fold(&self, [x, y]: [E; 2]) -> Result<E, [E; 2]> {
        match (x.into_terminal(), y.into_terminal()) {
            (Ok(disjunct1), Ok(disjunct2)) => Ok(E::terminal(disjunct1 || disjunct2)),
            // **disjunction** is _commutative_
            (Ok(val), Err(x)) | (Err(x), Ok(val)) => {
                if val {
                    Ok(E::tautology())
                } else {
                    Ok(E::partial(x))
                }
            }
            (Err(x), Err(y)) => Err([E::partial(x), E::partial(y)]),
        }
    }

    fn compose(&self, terms: [E; 2]) -> E {
        self.fold(terms).unwrap_or_else(|[x, y]| x | y)
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
