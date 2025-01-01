//! Nullary logical function (constant) equals to `true`.
//!
//! <https://en.wikipedia.org/wiki/Logical_truth>
use super::super::{Connective, Evaluable, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// A statement which is always true,
/// aka [tautology](https://en.wikipedia.org/wiki/Tautology_(logic)).
pub struct Truth;

// allow to use the `Truth` constant in unary or binary on N-ary context
impl<const ARITY: usize, E> TruthFn<ARITY, E> for Truth
where
    E: Evaluable,
{
    fn fold(&self, _: [E; ARITY]) -> Result<E, [E; ARITY]> {
        Ok(E::tautology())
    }

    fn compose(&self, _: [E; ARITY]) -> E {
        E::tautology()
    }
}

impl<const ARITY: usize> Connective<ARITY> for Truth {
    fn notation(&self) -> FunctionNotation {
        // _tee_ or _verum_
        // <https://en.wikipedia.org/wiki/Tee_(symbol)>
        '⊤'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Basic_logic_symbols
            'T'.into(),
            '1'.into(),
            FunctionNotation::common("true"),
            // probably derived from https://en.wikipedia.org/wiki/Veritas
            FunctionNotation::Polish('V'),
        ])
    }
}
