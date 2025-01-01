//! _Logical biconditional_, aka _material biconditional_
//! or _equivalence_ or _biimplication_ or _bientailment_,
//! is a logical operation that formally express
//! the notion of equality of its operands.
//!
//! <https://en.wikipedia.org/wiki/Logical_biconditional>
//! <https://en.wikipedia.org/wiki/Logical_equality>
use std::ops::{BitXor, Not};

use super::super::super::{Connective, Evaluable, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Logical biconditional is an operation on two logical values,
/// that produces a value of `true`
/// if and only if both operands are `false` or both operands are `true`.
pub struct LogicalBiconditional;

impl<E> TruthFn<2, E> for LogicalBiconditional
where
    E: Evaluable + Not<Output = E> + BitXor<Output = E>,
{
    fn fold(&self, [x, y]: [E; 2]) -> Result<E, [E; 2]> {
        match (x.into_terminal(), y.into_terminal()) {
            (Ok(x), Ok(y)) => Ok(E::terminal(x == y)),
            // **equivalence** is _commutative_
            (Ok(val), Err(x)) | (Err(x), Ok(val)) => {
                if val {
                    Ok(E::partial(x))
                } else {
                    Ok(!E::partial(x))
                }
            }
            (Err(x), Err(y)) => Err([E::partial(x), E::partial(y)]),
        }
    }

    fn compose(&self, terms: [E; 2]) -> E {
        self.fold(terms).unwrap_or_else(|[x, y]| !(x ^ y))
    }
}

impl Connective<2> for LogicalBiconditional {
    fn notation(&self) -> FunctionNotation {
        '↔'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '='.into(),
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Basic_logic_symbols
            '⇔'.into(),
            '≡'.into(),
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Advanced_or_rarely_used_logical_symbols
            '⊙'.into(),
            FunctionNotation::symbolic_str("⊃⊂"),
            FunctionNotation::symbolic_str("⊂⊃"),
            FunctionNotation::common("iff"),
            FunctionNotation::common("eq"),
            // https://en.wikipedia.org/wiki/XOR_gate
            FunctionNotation::scheme_gate("XNOR"),
            // short for Polish `ekwiwalencja` (1929)
            FunctionNotation::Polish('E'),
            // alternate Polish notation (1951)
            FunctionNotation::Polish('Q'),
        ])
    }
}
