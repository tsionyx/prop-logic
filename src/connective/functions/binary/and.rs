//! Logical conjunction is a binary operation that
//! is `true` if and only if all of its operands are true.
//!
//! <https://en.wikipedia.org/wiki/Logical_conjunction>
use std::ops::BitAnd;

use super::super::super::{Connective, Evaluable, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Logical conjunction is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// if and only if both of its operands are `true`.
pub struct Conjunction;

impl<E> TruthFn<2, E> for Conjunction
where
    E: Evaluable + BitAnd<Output = E>,
{
    fn fold(&self, [x, y]: [E; 2]) -> Result<E, [E; 2]> {
        match (x.into_terminal(), y.into_terminal()) {
            (Ok(conjunct1), Ok(conjunct2)) => Ok(E::terminal(conjunct1 && conjunct2)),
            // **conjunction** is _commutative_
            (Ok(val), Err(x)) | (Err(x), Ok(val)) => {
                if val {
                    Ok(E::partial(x))
                } else {
                    Ok(E::contradiction())
                }
            }
            (Err(x), Err(y)) => Err([E::partial(x), E::partial(y)]),
        }
    }

    fn compose(&self, terms: [E; 2]) -> E {
        self.fold(terms).unwrap_or_else(|[x, y]| x & y)
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
