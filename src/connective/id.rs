use super::{Formula, TruthFunction};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
/// The unary
/// [identity function](https://en.wikipedia.org/wiki/Identity_function)
/// for a proposition.
pub struct LogicalIdentity;

impl TruthFunction<1> for LogicalIdentity {
    fn eval(values: [bool; 1]) -> bool {
        // the truth value of the single given proposition
        // <https://en.wikipedia.org/wiki/Truth_value>
        let [value] = values;
        value
    }

    fn apply<T>([expr]: [Formula<T>; 1]) -> Formula<T> {
        expr
    }
}

// === The following implementation is degenerate ===
// impl Connective for LogicalIdentity {
//     const ARITY: usize = 1;
//
//     fn notation() -> FunctionNotation {
//         "Id".into()
//     }
// }
