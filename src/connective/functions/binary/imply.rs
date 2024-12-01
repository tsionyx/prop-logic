//! The _material conditional_ aka _material implication_
//! is a logical operation that formally express
//! conditional sentences in natural language.
//!
//! <https://en.wikipedia.org/wiki/Material_conditional>
use crate::formula::{Formula, Implies};

use super::super::{
    super::{Evaluation, FormulaComposer, Reducible},
    BoolFn, Connective, FunctionNotation,
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
            '≤'.into(),
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Basic_logic_symbols
            '⊃'.into(),
            '⇒'.into(),
            FunctionNotation::symbolic_str("->"),
            FunctionNotation::Polish('C'),
            // https://en.wikipedia.org/wiki/IMPLY_gate
            FunctionNotation::scheme_gate("IMPLY"),
        ])
    }
}
