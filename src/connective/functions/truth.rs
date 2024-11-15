//! Nullary logical function (constant) equals to `true`.
//!
//! <https://en.wikipedia.org/wiki/Logical_truth>
use super::{BoolFn, Connective, Formula, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// A statement which is always true,
/// aka [tautology](https://en.wikipedia.org/wiki/Tautology_(logic)).
pub struct Truth;

impl<const ARITY: usize> BoolFn<ARITY> for Truth {
    fn eval(&self, _values: [bool; ARITY]) -> bool {
        true
    }
}

// allow to use the `False` constant in unary or binary on N-ary context
impl<const ARITY: usize> TruthFn<ARITY> for Truth {
    fn init() -> Self {
        Self
    }

    fn apply<T>(&self, _expr: [Formula<T>; ARITY]) -> Formula<T> {
        Formula::tautology()
    }
}

impl Connective<0> for Truth {
    fn notation(&self) -> FunctionNotation {
        // _tee_ or _verum_
        // <https://en.wikipedia.org/wiki/Tee_(symbol)>
        '⊤'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec!["True".into(), "true".into(), "Vpq".into()])
    }
}
