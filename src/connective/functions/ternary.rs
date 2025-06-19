//! Generic ternary function as the composition of two binary functions.
//!
//! <https://en.wikipedia.org/wiki/Ternary_operation>
use crate::truth_table::TruthTabled;

use super::super::{ops::Associativity, Evaluable, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Wrapper for a ternary boolean function which applies
/// two binary functions in a specific order (LEFT or RIGHT).
pub struct Ternary<const LEFT: bool, Op1, Op2 = Op1> {
    op1: Op1,
    op2: Op2,
}

impl<const LEFT: bool, Op1, Op2> Ternary<LEFT, Op1, Op2> {
    /// Create an instance of the [`Ternary`] with given operators.
    pub const fn new(op1: Op1, op2: Op2) -> Self {
        Self { op1, op2 }
    }
}

impl<const LEFT: bool, E, Op1, Op2> TruthFn<3, E> for Ternary<LEFT, Op1, Op2>
where
    E: Evaluable + Clone, // TODO: try to get rid of this `Clone` requirement
    Op1: TruthFn<2, E> + TruthTabled<2> + Associativity,
    Op2: TruthFn<2, E> + TruthTabled<2, TT = <Op1 as TruthTabled<2>>::TT> + Associativity,
{
    fn try_reduce(&self, values: [E; 3]) -> Result<E, [E; 3]> {
        let try_fold = |left| {
            let [x, y, z] = values.clone();

            if left {
                let intermediate = self.op1.try_reduce([x, y]).ok()?;
                self.op2.try_reduce([intermediate, z]).ok()
            } else {
                let intermediate = self.op2.try_reduce([y, z]).ok()?;
                self.op1.try_reduce([x, intermediate]).ok()
            }
        };

        try_fold(LEFT)
            .or_else(|| {
                let can_switch = self.op1.is_equivalent(&self.op2) && self.op1.is_associative();
                if can_switch {
                    try_fold(!LEFT)
                } else {
                    None
                }
            })
            .ok_or(values)
    }

    fn compose(&self, [x, y, z]: [E; 3]) -> E {
        if LEFT {
            let intermediate = self.op1.compose([x, y]);
            self.op2.compose([intermediate, z])
        } else {
            let intermediate = self.op2.compose([y, z]);
            self.op1.compose([x, intermediate])
        }
    }

    fn eval(&self, [x, y, z]: [E; 3]) -> E {
        if LEFT {
            let intermediate = self.op1.eval([x, y]);
            self.op2.eval([intermediate, z])
        } else {
            let intermediate = self.op2.eval([y, z]);
            self.op1.eval([x, intermediate])
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Formula, Var};

    use super::{
        super::super::{
            functions::{Conjunction, Disjunction},
            InitFn as _, TruthFnConnector as _,
        },
        *,
    };

    #[test]
    fn ternary_conjunction() {
        let x = Ternary::<true, Conjunction>::init().connector();
        assert!(!x([false, false, false]));
        assert!(!x([false, false, true]));
        assert!(!x([false, true, false]));
        assert!(!x([false, true, true]));
        assert!(!x([true, false, false]));
        assert!(!x([true, false, true]));
        assert!(!x([true, true, false]));
        assert!(x([true, true, true]));
    }

    #[test]
    fn reduce_3_conjunction() {
        let x = Var::new(1);
        let y = Var::new(2);
        let falsity = Formula::contradiction();

        let left = Ternary::<true, Conjunction>::init();
        let right = Ternary::<false, Conjunction>::init();

        let res_left = left
            .try_reduce([x.into(), y.into(), falsity.clone()])
            .unwrap();
        assert!(res_left.is_contradiction());

        let res_right = right.try_reduce([x.into(), y.into(), falsity]).unwrap();
        assert!(res_right.is_contradiction());
    }

    #[test]
    fn reduce_3_disjunction() {
        let x = Var::new(1);
        let y = Var::new(2);
        let truth = Formula::tautology();

        let left = Ternary::<true, Disjunction>::init();
        let right = Ternary::<false, Disjunction>::init();

        let res_left = left
            .try_reduce([x.into(), y.into(), truth.clone()])
            .unwrap();
        assert!(res_left.is_tautology());

        let res_right = right.try_reduce([x.into(), y.into(), truth]).unwrap();
        assert!(res_right.is_tautology());
    }
}
