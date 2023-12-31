/// Nullary logical function (constant) equals to `true`.
///
/// <https://en.wikipedia.org/wiki/Logical_truth>
use super::{Connective, Formula, FunctionNotation, TruthFunction};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
/// A statement which is always true,
/// aka [tautology](https://en.wikipedia.org/wiki/Tautology_(logic)).
pub struct Truth;

// allow to use the `False` constant in unary or binary on N-ary context
impl<const ARITY: usize> TruthFunction<ARITY> for Truth {
    fn eval(_values: [bool; ARITY]) -> bool {
        true
    }

    fn apply<T>(_expr: [Formula<T>; ARITY]) -> Formula<T> {
        Formula::tautology()
    }
}

impl Connective for Truth {
    const ARITY: usize = 0;

    fn notation() -> FunctionNotation {
        // _tee_ or _verum_
        // <https://en.wikipedia.org/wiki/Tee_(symbol)>
        '⊤'.into()
    }

    fn alternate_notations() -> Option<Vec<FunctionNotation>> {
        Some(vec!["True".into(), "true".into(), "Vpq".into()])
    }
}
