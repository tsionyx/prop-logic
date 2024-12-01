//! Nullary logical function (constant) equals to `false`.
//!
//! <https://en.wikipedia.org/wiki/False_(logic)>
use super::{
    super::{Evaluation, FormulaComposer, Reducible},
    BoolFn, Connective, Formula, FunctionNotation,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// A statement which is always false,
/// aka [contradiction](https://en.wikipedia.org/wiki/Contradiction).
pub struct Falsity;

// allow to use the `False` constant in unary or binary on N-ary context
impl<const ARITY: usize> BoolFn<ARITY> for Falsity {
    fn eval(&self, _values: [bool; ARITY]) -> bool {
        false
    }
}

impl<const ARITY: usize, T> Reducible<ARITY, T> for Falsity {
    fn try_reduce(&self, _values: [Evaluation<T>; ARITY]) -> Option<Evaluation<T>> {
        Some(Evaluation::contradiction())
    }
}

impl<const ARITY: usize, T> FormulaComposer<ARITY, T> for Falsity {
    fn compose(&self, _expr: [Formula<T>; ARITY]) -> Formula<T> {
        Formula::contradiction()
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
