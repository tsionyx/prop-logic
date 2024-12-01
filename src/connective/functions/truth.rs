//! Nullary logical function (constant) equals to `true`.
//!
//! <https://en.wikipedia.org/wiki/Logical_truth>
use super::{
    super::{Evaluation, FormulaComposer, Reducible},
    BoolFn, Connective, Formula, FunctionNotation,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// A statement which is always true,
/// aka [tautology](https://en.wikipedia.org/wiki/Tautology_(logic)).
pub struct Truth;

impl<const ARITY: usize> BoolFn<ARITY> for Truth {
    fn eval(&self, _values: [bool; ARITY]) -> bool {
        true
    }
}

impl<const ARITY: usize, T> Reducible<ARITY, T> for Truth {
    fn try_reduce(&self, _values: [Evaluation<T>; ARITY]) -> Option<Evaluation<T>> {
        Some(Evaluation::tautology())
    }
}

impl<const ARITY: usize, T> FormulaComposer<ARITY, T> for Truth {
    fn compose(&self, _expr: [Formula<T>; ARITY]) -> Formula<T> {
        Formula::tautology()
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
