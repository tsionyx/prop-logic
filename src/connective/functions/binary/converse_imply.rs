//! The _converse conditional_ aka _converse implication_
//! is a logical operation that is the result of
//! reversing the [material implication][super::imply]'s
//! _antecedent_ and _consequent_.
//!
//! <https://en.wikipedia.org/wiki/Converse_implication>
use crate::formula::{Formula, Implies};

use super::super::{
    super::{Evaluation, FormulaComposer, Reducible},
    BoolFn, Connective, FunctionNotation,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Converse implication is an operation on two logical values,
/// that produces a value of `true`
/// unless its first argument is `false` and its second argument is `true`.
pub struct ConverseImplication;

impl BoolFn<2> for ConverseImplication {
    fn eval(&self, [consequent, antecedent]: [bool; 2]) -> bool {
        consequent || !antecedent
    }
}

impl<T> Reducible<2, T> for ConverseImplication
where
    T: std::ops::Not<Output = T>,
{
    fn try_reduce(&self, values: [Evaluation<T>; 2]) -> Option<Evaluation<T>> {
        use Evaluation::{Partial, Terminal};
        match values {
            [Partial(_), Partial(_)] => None,
            [Partial(consequent), Terminal(antecedent)] => {
                if antecedent {
                    Some(Partial(consequent))
                } else {
                    Some(Evaluation::tautology())
                }
            }
            [Terminal(consequent), Partial(antecedent)] => {
                if consequent {
                    Some(Evaluation::tautology())
                } else {
                    Some(Partial(!antecedent))
                }
            }
            [Terminal(val1), Terminal(val2)] => Some(Terminal(self.eval([val1, val2]))),
        }
    }
}

impl<T> FormulaComposer<2, T> for ConverseImplication {
    fn compose(&self, [consequent, antecedent]: [Formula<T>; 2]) -> Formula<T> {
        antecedent.implies(consequent)
    }
}

impl Connective<2> for ConverseImplication {
    fn notation(&self) -> FunctionNotation {
        '←'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '≥'.into(),
            '⊂'.into(),
            // https://en.wikipedia.org/wiki/Arrows_(Unicode_block)
            '⇐'.into(),
            FunctionNotation::symbolic_str("<-"),
            FunctionNotation::Polish('B'),
        ])
    }
}
