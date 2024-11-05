use super::{Connective, Formula, FunctionNotation, TruthFunction};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
/// The unary
/// [identity function](https://en.wikipedia.org/wiki/Identity_function)
/// for a proposition.
pub struct LogicalIdentity;

impl TruthFunction<1> for LogicalIdentity {
    fn init() -> Self {
        Self
    }

    fn eval(&self, values: [bool; 1]) -> bool {
        // the truth value of the single given proposition
        // <https://en.wikipedia.org/wiki/Truth_value>
        let [value] = values;
        value
    }

    fn apply<T>(&self, [expr]: [Formula<T>; 1]) -> Formula<T> {
        expr
    }
}

impl Connective<1> for LogicalIdentity {
    fn notation(&self) -> FunctionNotation {
        // degenerate implementation
        "".into()
    }
}
