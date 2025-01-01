//! Nullary logical function (constant) equals to `false`.
//!
//! <https://en.wikipedia.org/wiki/False_(logic)>
use super::super::{Connective, Evaluable, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// A statement which is always false,
/// aka [contradiction](https://en.wikipedia.org/wiki/Contradiction).
pub struct Falsity;

// allow to use the `Falsity` constant in unary or binary on N-ary context
impl<const ARITY: usize, E> TruthFn<ARITY, E> for Falsity
where
    E: Evaluable,
{
    fn fold(&self, _: [E; ARITY]) -> Result<E, [E; ARITY]> {
        Ok(E::contradiction())
    }

    fn compose(&self, _: [E; ARITY]) -> E {
        E::contradiction()
    }
}

impl<const ARITY: usize> Connective<ARITY> for Falsity {
    fn notation(&self) -> FunctionNotation {
        // _falsum_ or _absurdum_
        // <https://en.wikipedia.org/wiki/Up_tack>
        '⊥'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Basic_logic_symbols
            'F'.into(),
            '0'.into(),
            FunctionNotation::common("false"),
            // probably derived from visual similarity to '0'
            FunctionNotation::Polish('O'),
        ])
    }
}
