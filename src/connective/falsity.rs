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
    fn eval(_values: [bool; ARITY]) -> bool {
        false
    }

    fn apply<T>(_expr: [Formula<T>; ARITY]) -> Formula<T> {
        Formula::contradiction()
    }
}

impl Connective for Falsity {
    const ARITY: usize = 0;

    fn notation() -> FunctionNotation {
        // _falsum_ or _absurdum_
        // <https://en.wikipedia.org/wiki/Up_tack>
        '⊥'.into()
    }

    fn alternate_notations() -> Option<Vec<FunctionNotation>> {
        Some(vec!["False".into(), "false".into(), "Opq".into()])
    }
}
