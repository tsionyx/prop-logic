//! Generic ternary function as the composition of two binary functions.
//!
//! <https://en.wikipedia.org/wiki/Ternary_operation>
use super::super::{Evaluable, TruthFn};

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

impl<const LEFT: bool, E, Op1, Op2> TruthFn<3, E> for Ternary<LEFT, Op1, Op2>
where
    E: Evaluable + Clone, // TODO: try to get rid of this `Clone` requirement
    Op1: TruthFn<2, E>,
    Op2: TruthFn<2, E>,
{
    fn fold(&self, values: [E; 3]) -> Result<E, [E; 3]> {
        let [x, y, z] = values.clone();

        // TODO: consider for example the case of (expr1 || expr2 || false)
        let optional = || {
            if LEFT {
                let intermediate = self.op1.fold([x, y]).ok()?;
                self.op2.fold([intermediate, z]).ok()
            } else {
                let intermediate = self.op2.fold([y, z]).ok()?;
                self.op1.fold([x, intermediate]).ok()
            }
        };

        optional().ok_or(values)
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
}

#[cfg(test)]
mod tests {
    use super::{
        super::super::{
            functions::{
                AllEquivalent, Conjunction, ConjunctionAny, Disjunction, DisjunctionAny,
                ExclusiveDisjunction, ExclusiveDisjunctionAny, LogicalBiconditional,
            },
            BoolFn, InitFn as _, TruthFnConnector as _, TruthTable,
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
    fn equivalent_to_3_conjunction() {
        let x = Ternary::<true, Conjunction>::init().get_truth_table();
        let y = Ternary::<false, Conjunction>::init().get_truth_table();
        assert_eq!(x.values(), y.values());
        let z: TruthTable<3> = ConjunctionAny::init().get_truth_table();
        assert_eq!(x.values(), z.values());
        assert_eq!(y.into_inner(), z.into_inner());
    }

    #[test]
    fn equivalent_to_3_disjunction() {
        let x = Ternary::<true, Disjunction>::init().get_truth_table();
        let y = Ternary::<false, Disjunction>::init().get_truth_table();
        assert_eq!(x.values(), y.values());
        let z: TruthTable<3> = DisjunctionAny::init().get_truth_table();
        assert_eq!(x.values(), z.values());
        assert_eq!(y.into_inner(), z.into_inner());
    }

    #[test]
    fn equivalent_to_3_xor() {
        let x = Ternary::<true, ExclusiveDisjunction>::init().get_truth_table();
        let y = Ternary::<false, ExclusiveDisjunction>::init().get_truth_table();
        assert_eq!(x.values(), y.values());
        let z: TruthTable<3> = ExclusiveDisjunctionAny::init().get_truth_table();
        assert_eq!(x.values(), z.values());
        assert_eq!(y.into_inner(), z.into_inner());
    }

    #[test]
    #[should_panic(expected = "left == right")] // the 'all equal' is stronger than 'pairwise equal'
    fn equivalent_to_3_eq() {
        let x = Ternary::<true, LogicalBiconditional>::init().get_truth_table();
        let y = Ternary::<false, LogicalBiconditional>::init().get_truth_table();
        assert_eq!(x.values(), y.values());
        let z: TruthTable<3> = AllEquivalent::init().get_truth_table();
        assert_eq!(x.values(), z.values());
        assert_eq!(y.into_inner(), z.into_inner());
    }
}
