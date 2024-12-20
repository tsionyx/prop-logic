//! The _converse nonimplication_
//! is a logical operation that is the
//! [negation][super::neg] of [converse implication][super::converse_imply]
//!
//! (equivalently, the [negation][super::neg]
//! of the converse of [implication][super::imply]).
//!
//! <https://en.wikipedia.org/wiki/Converse_nonimplication>
use crate::formula::{Formula, Implies};

use super::super::{
    super::{Evaluation, FormulaComposer, Reducible},
    BoolFn, Connective, FunctionNotation,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Converse nonimplication is an operation on two logical values,
/// that produces a value of `false`
/// unless its first argument is `false` and its second argument is `true`.
pub struct ConverseNonImplication;

impl BoolFn<2> for ConverseNonImplication {
    fn eval(&self, [consequent, antecedent]: [bool; 2]) -> bool {
        !consequent && antecedent
    }
}

impl<T> Reducible<2, T> for ConverseNonImplication
where
    T: std::ops::Not<Output = T>,
{
    fn try_reduce(&self, values: [Evaluation<T>; 2]) -> Option<Evaluation<T>> {
        use Evaluation::{Partial, Terminal};
        match values {
            [Partial(_), Partial(_)] => None,
            [Partial(consequent), Terminal(antecedent)] => {
                if antecedent {
                    Some(Partial(!consequent))
                } else {
                    Some(Evaluation::contradiction())
                }
            }
            [Terminal(consequent), Partial(antecedent)] => {
                if consequent {
                    Some(Evaluation::contradiction())
                } else {
                    Some(Partial(antecedent))
                }
            }
            [Terminal(val1), Terminal(val2)] => Some(Terminal(self.eval([val1, val2]))),
        }
    }
}

impl<T> FormulaComposer<2, T> for ConverseNonImplication {
    fn compose(&self, [consequent, antecedent]: [Formula<T>; 2]) -> Formula<T> {
        !(antecedent.implies(consequent))
    }
}

impl Connective<2> for ConverseNonImplication {
    fn notation(&self) -> FunctionNotation {
        '↚'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '<'.into(),
            '⊄'.into(),
            // https://en.wikipedia.org/wiki/Arrows_(Unicode_block)
            '⇍'.into(),
            FunctionNotation::Polish('M'),
        ])
    }
}
