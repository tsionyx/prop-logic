//! The _material conditional_ aka _material implication_
//! is a logical operation that formally express
//! conditional sentences in natural language.
//!
//! <https://en.wikipedia.org/wiki/Material_conditional>
use crate::formula::{Formula, Implies};

use super::{
    super::{Evaluation, FormulaComposer, Reducible},
    BoolFn, Connective, FunctionNotation, TruthFn,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Material implication is an operation on two logical values,
/// that produces a value of `true`
/// unless its first argument is `true` and its second argument is `false`.
pub struct MaterialImplication;

impl BoolFn<2> for MaterialImplication {
    fn eval(&self, [antecedent, consequent]: [bool; 2]) -> bool {
        !antecedent || consequent
    }
}

impl TruthFn<2> for MaterialImplication {
    fn init() -> Self {
        Self
    }
}

impl<T> Reducible<2, T> for MaterialImplication
where
    T: std::ops::Not<Output = T>,
{
    fn try_reduce(&self, values: [Evaluation<T>; 2]) -> Option<Evaluation<T>> {
        use Evaluation::{Partial, Terminal};
        match values {
            [Partial(_), Partial(_)] => None,
            [Partial(antecedent), Terminal(consequent)] => {
                if consequent {
                    Some(Evaluation::tautology())
                } else {
                    Some(Partial(!antecedent))
                }
            }
            [Terminal(antecedent), Partial(consequent)] => {
                if antecedent {
                    Some(Partial(consequent))
                } else {
                    Some(Evaluation::tautology())
                }
            }
            [Terminal(val1), Terminal(val2)] => Some(Terminal(self.eval([val1, val2]))),
        }
    }
}

impl<T> FormulaComposer<2, T> for MaterialImplication {
    fn compose(&self, [antecedent, consequent]: [Formula<T>; 2]) -> Formula<T> {
        antecedent.implies(consequent)
    }
}

impl Connective<2> for MaterialImplication {
    fn notation(&self) -> FunctionNotation {
        '→'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '⊃'.into(),
            '⇒'.into(),
            '≤'.into(),
            "Cpq".into(), // Polish notation
            "IMPLY".into(),
        ])
    }
}
