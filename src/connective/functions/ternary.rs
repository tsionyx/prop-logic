//! Generic ternary function as the composition of two binary functions.
//!
//! <https://en.wikipedia.org/wiki/Ternary_operation>
use super::{
    super::{Evaluation, FormulaComposer, Reducible},
    BoolFn, Formula, TruthFn,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Wrapper for a ternary boolean function which applies
/// two binary functions in a specific order (LEFT or RIGHT).
pub struct Ternary<const LEFT: bool, Op1, Op2 = Op1> {
    op1: Op1,
    op2: Op2,
}

impl<const LEFT: bool, Op1, Op2> Ternary<LEFT, Op1, Op2> {
    /// Create an instance of the [`ProjectAndUnary`].
    pub const fn new(op1: Op1, op2: Op2) -> Self {
        Self { op1, op2 }
    }
}

impl<const LEFT: bool, Op1, Op2> BoolFn<3> for Ternary<LEFT, Op1, Op2>
where
    Op1: BoolFn<2>,
    Op2: BoolFn<2>,
{
    fn eval(&self, [x, y, z]: [bool; 3]) -> bool {
        if LEFT {
            let intermediate = self.op1.eval([x, y]);
            self.op2.eval([intermediate, z])
        } else {
            let intermediate = self.op2.eval([y, z]);
            self.op1.eval([x, intermediate])
        }
    }
}

impl<const LEFT: bool, Op1, Op2, T> Reducible<3, T> for Ternary<LEFT, Op1, Op2>
where
    Op1: TruthFn<2> + Reducible<2, T>,
    Op2: TruthFn<2> + Reducible<2, T>,
{
    fn try_reduce(&self, [x, y, z]: [Evaluation<T>; 3]) -> Option<Evaluation<T>> {
        // TODO: consider for example the case of (expr1 || expr2 || false)
        if LEFT {
            let intermediate = self.op1.try_reduce([x, y])?;
            self.op2.try_reduce([intermediate, z])
        } else {
            let intermediate = self.op2.try_reduce([y, z])?;
            self.op1.try_reduce([x, intermediate])
        }
    }
}

impl<const LEFT: bool, Op1, Op2, T> FormulaComposer<3, T> for Ternary<LEFT, Op1, Op2>
where
    Op1: TruthFn<2> + FormulaComposer<2, T>,
    Op2: TruthFn<2> + FormulaComposer<2, T>,
{
    fn compose(&self, [x, y, z]: [Formula<T>; 3]) -> Formula<T>
    where
        Op1: Sized,
        Op2: Sized,
    {
        if LEFT {
            let intermediate = self.op1.compose([x, y]);
            self.op2.compose([intermediate, z])
        } else {
            let intermediate = self.op2.compose([y, z]);
            self.op1.compose([x, intermediate])
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::connective::Conjunction;

    use super::*;

    #[test]
    fn ternary_conjunction() {
        let x = Ternary::<true, Conjunction>::init().bool_evaluator();
        assert!(!x([false, false, false]));
        assert!(!x([false, false, true]));
        assert!(!x([false, true, false]));
        assert!(!x([false, true, true]));
        assert!(!x([true, false, false]));
        assert!(!x([true, false, true]));
        assert!(!x([true, true, false]));
        assert!(x([true, true, true]));
    }
}
