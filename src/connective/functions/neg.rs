//! Negation aka logical NOT aka logical complement,
//! is an unary operation that reverse its only argument.
//!
//! <https://en.wikipedia.org/wiki/Negation>
use super::{Connective, Formula, FunctionNotation, TruthFunction};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
/// Unary operation that takes a proposition P
/// to another proposition "not P"
/// with [its value](https://en.wikipedia.org/wiki/Truth_value)
/// switched.
pub struct Negation;

impl TruthFunction<1> for Negation {
    fn init() -> Self {
        Self
    }

    fn eval(&self, values: [bool; 1]) -> bool {
        let [value] = values;
        !value
    }

    fn apply<T>(&self, [expr]: [Formula<T>; 1]) -> Formula<T> {
        !expr
    }
}

impl Connective<1> for Negation {
    fn notation(&self) -> FunctionNotation {
        '¬'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '~'.into(),
            '-'.into(),
            '!'.into(),
            "Npq".into(), // short for Polish `negacja`
            "NOT".into(),
            "not".into(),
        ])
    }
}
