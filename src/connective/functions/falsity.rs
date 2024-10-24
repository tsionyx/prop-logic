//! Nullary logical function (constant) equals to `false`.
//!
//! <https://en.wikipedia.org/wiki/False_(logic)>
use super::{Connective, Formula, FunctionNotation, TruthFunction};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
/// A statement which is always false,
/// aka [contradiction](https://en.wikipedia.org/wiki/Contradiction).
pub struct Falsity;

// allow to use the `False` constant in unary or binary on N-ary context
impl<const ARITY: usize> TruthFunction<ARITY> for Falsity {
    fn init() -> Self {
        Self
    }

    fn eval(&self, _values: [bool; ARITY]) -> bool {
        false
    }

    fn apply<T>(&self, _expr: [Formula<T>; ARITY]) -> Formula<T> {
        Formula::contradiction()
    }
}

impl Connective<0> for Falsity {
    fn notation(&self) -> FunctionNotation {
        // _falsum_ or _absurdum_
        // <https://en.wikipedia.org/wiki/Up_tack>
        '⊥'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec!["False".into(), "false".into(), "Opq".into()])
    }
}
