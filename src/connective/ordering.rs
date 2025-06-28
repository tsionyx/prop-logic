use std::cmp::Ordering;

#[allow(clippy::wildcard_imports)]
use super::{functions::*, BoolFn, InitFn as _};

use crate::{
    arity::TwoPower,
    truth_table::{TruthTable as _, TruthTabled as _},
    utils::{cartesian_diag, dependent_array::SizeMapper},
};

/// Defines the partial order (`<=`) between the [`TruthFn`]-s
/// by associating them with the sets.
///
/// For more information, see the
/// [Haase diagram](https://en.wikipedia.org/wiki/Logical_connective#Table_and_Hasse_diagram).
fn partial_ordering<Op1, Op2, const ARITY: usize>() -> Option<Ordering>
where
    Op1: BoolFn<ARITY> + Default,
    Op2: BoolFn<ARITY> + Default,
    TwoPower: SizeMapper<ARITY>,
{
    let op1_truth_table = Op1::init().get_truth_table().values();
    let op2_truth_table = Op2::init().get_truth_table().values();

    let mut set_ordering = Ordering::Equal;
    for (&val1, &val2) in op1_truth_table.iter().zip(&op2_truth_table) {
        match set_ordering {
            Ordering::Greater => {
                // considered strict superset and no `(false, true)`
                // were previously found
                if !val1 && val2 {
                    return None;
                }
            }
            Ordering::Less => {
                // considered strict subset and no `(true, false)`
                // were previously found
                if val1 && !val2 {
                    return None;
                }
            }
            Ordering::Equal => {
                set_ordering = match (val1, val2) {
                    (false, false) | (true, true) => Ordering::Equal,
                    (false, true) => Ordering::Less,
                    (true, false) => Ordering::Greater,
                }
            }
        }
    }

    Some(set_ordering)
}

macro_rules! def_partial_ord {
    ($t1:ty: $arity:literal => $($t2:ty),+ $(,)? ) => {
        $(
            impl PartialEq<$t2> for $t1 {
                fn eq(&self, _other: &$t2) -> bool {
                    use std::any::TypeId;
                    TypeId::of::<$t1>() == TypeId::of::<$t2>()
                }
            }

            impl PartialOrd<$t2> for $t1 {
                fn partial_cmp(&self, _other: &$t2) -> Option<Ordering> {
                    partial_ordering::<Self, $t2, $arity>()
                }
            }
        )+
    };
}

// unary
macro_rules! unary_partial_ord {
    ($t1: tt, $t2: tt) => {
        def_partial_ord! {$t1: 1 => $t2}
    };
}

cartesian_diag!(
    [Truth, Falsity],
    [LogicalIdentity, Negation],
    unary_partial_ord
);

def_partial_ord!(LogicalIdentity: 1 => Truth, Falsity, Negation);
def_partial_ord!(Negation: 1 => Truth, Falsity, LogicalIdentity);

// binary
macro_rules! binary_partial_ord {
    ($t1: tt, $t2: tt) => {
        def_partial_ord! {$t1: 2 => $t2}
    };
}

macro_rules! ignore {
    ($a: tt, $b: tt) => {};
}

cartesian_diag!(
    square
    [
        Truth,
        Falsity,
        First,
        Last,
        NotFirst,
        NotSecond,
        Conjunction,
        NonConjunction,
        Disjunction,
        NonDisjunction,
        ExclusiveDisjunction,
        LogicalBiconditional,
        MaterialImplication,
        MaterialNonImplication,
        ConverseImplication,
        ConverseNonImplication
    ],
    binary_partial_ord,
    ignore
);

#[cfg(test)]
mod order_tests {
    use super::*;

    #[test]
    fn unary_identity() {
        assert!(LogicalIdentity > Falsity);
        assert!(Falsity < LogicalIdentity);
        assert!(LogicalIdentity.partial_cmp(&Negation).is_none());
        assert!(LogicalIdentity < Truth);
        assert!(Truth > LogicalIdentity);
    }

    #[test]
    fn unary_negation() {
        assert!(Negation > Falsity);
        assert!(Falsity < Negation);
        assert!(Negation.partial_cmp(&LogicalIdentity).is_none());
        assert!(Negation < Truth);
        assert!(Truth > Negation);
    }

    #[test]
    fn tautology_is_greatest() {
        assert_eq!(Truth, Truth);
        assert!(Truth > NonConjunction);
        assert!(Truth > ConverseImplication);
        assert!(Truth > MaterialImplication);
        assert!(Truth > Disjunction);
        assert!(Truth > NotSecond::new());
        assert!(Truth > NotFirst::new());
        assert!(Truth > ExclusiveDisjunction);
        assert!(Truth > LogicalBiconditional);
        assert!(Truth > First {});
        assert!(Truth > Last {});
        assert!(Truth > NonDisjunction);
        assert!(Truth > MaterialNonImplication);
        assert!(Truth > ConverseNonImplication);
        assert!(Truth > Conjunction);
        assert!(Truth > Falsity);
    }

    #[test]
    fn sheffer_stroke() {
        assert!(NonConjunction < Truth);
        assert_eq!(NonConjunction, NonConjunction);
        assert!(NonConjunction.partial_cmp(&ConverseImplication).is_none());
        assert!(NonConjunction.partial_cmp(&MaterialImplication).is_none());
        assert!(NonConjunction.partial_cmp(&Disjunction).is_none());
        assert!(NonConjunction > NotSecond::new());
        assert!(NonConjunction > NotFirst::new());
        assert!(NonConjunction > ExclusiveDisjunction);
        assert!(NonConjunction.partial_cmp(&LogicalBiconditional).is_none());
        assert!(NonConjunction.partial_cmp(&First {}).is_none());
        assert!(NonConjunction.partial_cmp(&Last {}).is_none());
        assert!(NonConjunction > NonDisjunction);
        assert!(NonConjunction > MaterialNonImplication);
        assert!(NonConjunction > ConverseNonImplication);
        assert!(NonConjunction.partial_cmp(&Conjunction).is_none());
        assert!(NonConjunction > Falsity);
    }

    #[test]
    fn converse_implication() {
        assert!(ConverseImplication < Truth);
        assert!(ConverseImplication.partial_cmp(&NonConjunction).is_none());
        assert_eq!(ConverseImplication, ConverseImplication);
        assert!(ConverseImplication
            .partial_cmp(&MaterialImplication)
            .is_none());
        assert!(ConverseImplication.partial_cmp(&Disjunction).is_none());
        assert!(ConverseImplication > NotSecond::new());
        assert!(ConverseImplication.partial_cmp(&NotFirst::new()).is_none());
        assert!(ConverseImplication
            .partial_cmp(&ExclusiveDisjunction)
            .is_none());
        assert!(ConverseImplication > LogicalBiconditional);
        assert!(ConverseImplication > First {});
        assert!(ConverseImplication.partial_cmp(&Last {}).is_none());
        assert!(ConverseImplication > NonDisjunction);
        assert!(ConverseImplication > MaterialNonImplication);
        assert!(ConverseImplication
            .partial_cmp(&ConverseNonImplication)
            .is_none());
        assert!(ConverseImplication > Conjunction);
        assert!(ConverseImplication > Falsity);
    }

    #[test]
    fn implication() {
        assert!(MaterialImplication < Truth);
        assert!(MaterialImplication.partial_cmp(&NonConjunction).is_none());
        assert!(MaterialImplication
            .partial_cmp(&ConverseImplication)
            .is_none());
        assert_eq!(MaterialImplication, MaterialImplication);
        assert!(MaterialImplication.partial_cmp(&Disjunction).is_none());
        assert!(MaterialImplication.partial_cmp(&NotSecond::new()).is_none());
        assert!(MaterialImplication > NotFirst::new());
        assert!(MaterialImplication
            .partial_cmp(&ExclusiveDisjunction)
            .is_none());
        assert!(MaterialImplication > LogicalBiconditional);
        assert!(MaterialImplication.partial_cmp(&First {}).is_none());
        assert!(MaterialImplication > Last {});
        assert!(MaterialImplication > NonDisjunction);
        assert!(MaterialImplication
            .partial_cmp(&MaterialNonImplication)
            .is_none());
        assert!(MaterialImplication > ConverseNonImplication);
        assert!(MaterialImplication > Conjunction);
        assert!(MaterialImplication > Falsity);
    }

    #[test]
    fn disjunction() {
        assert!(Disjunction < Truth);
        assert!(Disjunction.partial_cmp(&NonConjunction).is_none());
        assert!(Disjunction.partial_cmp(&ConverseImplication).is_none());
        assert!(Disjunction.partial_cmp(&MaterialImplication).is_none());
        assert_eq!(Disjunction, Disjunction);
        assert!(Disjunction.partial_cmp(&NotSecond::new()).is_none());
        assert!(Disjunction.partial_cmp(&NotFirst::new()).is_none());
        assert!(Disjunction > ExclusiveDisjunction);
        assert!(Disjunction.partial_cmp(&LogicalBiconditional).is_none());
        assert!(Disjunction > First {});
        assert!(Disjunction > Last {});
        assert!(Disjunction.partial_cmp(&NonDisjunction).is_none());
        assert!(Disjunction > MaterialNonImplication);
        assert!(Disjunction > ConverseNonImplication);
        assert!(Disjunction > Conjunction);
        assert!(Disjunction > Falsity);
    }

    #[test]
    fn neg_right_projection() {
        let neg_right = NotSecond::new();
        assert!(neg_right < Truth);
        assert!(neg_right < NonConjunction);
        assert!(neg_right < ConverseImplication);
        assert!(neg_right.partial_cmp(&MaterialImplication).is_none());
        assert!(neg_right.partial_cmp(&Disjunction).is_none());
        assert_eq!(neg_right, NotSecond::new());
        assert!(neg_right.partial_cmp(&NotFirst::new()).is_none());
        assert!(neg_right.partial_cmp(&ExclusiveDisjunction).is_none());
        assert!(neg_right.partial_cmp(&LogicalBiconditional).is_none());
        assert!(neg_right.partial_cmp(&First {}).is_none());
        assert!(neg_right.partial_cmp(&Last {}).is_none());
        assert!(neg_right > NonDisjunction);
        assert!(neg_right > MaterialNonImplication);
        assert!(neg_right.partial_cmp(&ConverseNonImplication).is_none());
        assert!(neg_right.partial_cmp(&Conjunction).is_none());
        assert!(neg_right > Falsity);
    }

    #[test]
    fn neg_left_projection() {
        let neg_left = NotFirst::new();
        assert!(neg_left < Truth);
        assert!(neg_left < NonConjunction);
        assert!(neg_left.partial_cmp(&ConverseImplication).is_none());
        assert!(neg_left < MaterialImplication);
        assert!(neg_left.partial_cmp(&Disjunction).is_none());
        assert!(neg_left.partial_cmp(&NotSecond::new()).is_none());
        assert_eq!(neg_left, NotFirst::new());
        assert!(neg_left.partial_cmp(&ExclusiveDisjunction).is_none());
        assert!(neg_left.partial_cmp(&LogicalBiconditional).is_none());
        assert!(neg_left.partial_cmp(&First {}).is_none());
        assert!(neg_left.partial_cmp(&Last {}).is_none());
        assert!(neg_left > NonDisjunction);
        assert!(neg_left.partial_cmp(&MaterialNonImplication).is_none());
        assert!(neg_left > ConverseNonImplication);
        assert!(neg_left.partial_cmp(&Conjunction).is_none());
        assert!(neg_left > Falsity);
    }

    #[test]
    fn xor() {
        assert!(ExclusiveDisjunction < Truth);
        assert!(ExclusiveDisjunction < NonConjunction);
        assert!(ExclusiveDisjunction
            .partial_cmp(&ConverseImplication)
            .is_none());
        assert!(ExclusiveDisjunction
            .partial_cmp(&MaterialImplication)
            .is_none());
        assert!(ExclusiveDisjunction < Disjunction);
        assert!(ExclusiveDisjunction
            .partial_cmp(&NotSecond::new())
            .is_none());
        assert!(ExclusiveDisjunction.partial_cmp(&NotFirst::new()).is_none());
        assert_eq!(ExclusiveDisjunction, ExclusiveDisjunction);
        assert!(ExclusiveDisjunction
            .partial_cmp(&LogicalBiconditional)
            .is_none());
        assert!(ExclusiveDisjunction.partial_cmp(&First {}).is_none());
        assert!(ExclusiveDisjunction.partial_cmp(&Last {}).is_none());
        assert!(ExclusiveDisjunction.partial_cmp(&NonDisjunction).is_none());
        assert!(ExclusiveDisjunction > MaterialNonImplication);
        assert!(ExclusiveDisjunction > ConverseNonImplication);
        assert!(ExclusiveDisjunction.partial_cmp(&Conjunction).is_none());
        assert!(ExclusiveDisjunction > Falsity);
    }

    #[test]
    fn equiv() {
        assert!(LogicalBiconditional < Truth);
        assert!(LogicalBiconditional.partial_cmp(&NonConjunction).is_none());
        assert!(LogicalBiconditional < ConverseImplication);
        assert!(LogicalBiconditional < MaterialImplication);
        assert!(LogicalBiconditional.partial_cmp(&Disjunction).is_none());
        assert!(LogicalBiconditional
            .partial_cmp(&NotSecond::new())
            .is_none());
        assert!(LogicalBiconditional.partial_cmp(&NotFirst::new()).is_none());
        assert!(LogicalBiconditional
            .partial_cmp(&ExclusiveDisjunction)
            .is_none());
        assert_eq!(LogicalBiconditional, LogicalBiconditional);
        assert!(LogicalBiconditional.partial_cmp(&First {}).is_none());
        assert!(LogicalBiconditional.partial_cmp(&Last {}).is_none());
        assert!(LogicalBiconditional > NonDisjunction);
        assert!(LogicalBiconditional
            .partial_cmp(&MaterialNonImplication)
            .is_none());
        assert!(LogicalBiconditional
            .partial_cmp(&ConverseNonImplication)
            .is_none());
        assert!(LogicalBiconditional > Conjunction);
        assert!(LogicalBiconditional > Falsity);
    }

    #[test]
    fn left_projection() {
        let left = First {};
        assert!(left < Truth);
        assert!(left.partial_cmp(&NonConjunction).is_none());
        assert!(left < ConverseImplication);
        assert!(left.partial_cmp(&MaterialImplication).is_none());
        assert!(left < Disjunction);
        assert!(left.partial_cmp(&NotSecond::new()).is_none());
        assert!(left.partial_cmp(&NotFirst::new()).is_none());
        assert!(left.partial_cmp(&ExclusiveDisjunction).is_none());
        assert!(left.partial_cmp(&LogicalBiconditional).is_none());
        assert_eq!(left, First {});
        assert!(left.partial_cmp(&Last {}).is_none());
        assert!(left.partial_cmp(&NonDisjunction).is_none());
        assert!(left > MaterialNonImplication);
        assert!(left.partial_cmp(&ConverseNonImplication).is_none());
        assert!(left > Conjunction);
        assert!(left > Falsity);
    }

    #[test]
    fn right_projection() {
        let right = Last {};
        assert!(right < Truth);
        assert!(right.partial_cmp(&NonConjunction).is_none());
        assert!(right.partial_cmp(&ConverseImplication).is_none());
        assert!(right < MaterialImplication);
        assert!(right < Disjunction);
        assert!(right.partial_cmp(&NotSecond::new()).is_none());
        assert!(right.partial_cmp(&NotFirst::new()).is_none());
        assert!(right.partial_cmp(&ExclusiveDisjunction).is_none());
        assert!(right.partial_cmp(&LogicalBiconditional).is_none());
        assert!(right.partial_cmp(&First {}).is_none());
        assert_eq!(right, Last {});
        assert!(right.partial_cmp(&NonDisjunction).is_none());
        assert!(right.partial_cmp(&MaterialNonImplication).is_none());
        assert!(right > ConverseNonImplication);
        assert!(right > Conjunction);
        assert!(right > Falsity);
    }

    #[test]
    fn pierce_arrow() {
        assert!(NonDisjunction < Truth);
        assert!(NonDisjunction < NonConjunction);
        assert!(NonDisjunction < ConverseImplication);
        assert!(NonDisjunction < MaterialImplication);
        assert!(NonDisjunction.partial_cmp(&Disjunction).is_none());
        assert!(NonDisjunction < NotSecond::new());
        assert!(NonDisjunction < NotFirst::new());
        assert!(NonDisjunction.partial_cmp(&ExclusiveDisjunction).is_none());
        assert!(NonDisjunction < LogicalBiconditional);
        assert!(NonDisjunction.partial_cmp(&First {}).is_none());
        assert!(NonDisjunction.partial_cmp(&Last {}).is_none());
        assert_eq!(NonDisjunction, NonDisjunction);
        assert!(NonDisjunction
            .partial_cmp(&MaterialNonImplication)
            .is_none());
        assert!(NonDisjunction
            .partial_cmp(&ConverseNonImplication)
            .is_none());
        assert!(NonDisjunction.partial_cmp(&Conjunction).is_none());
        assert!(NonDisjunction > Falsity);
    }

    #[test]
    fn non_imply() {
        assert!(MaterialNonImplication < Truth);
        assert!(MaterialNonImplication < NonConjunction);
        assert!(MaterialNonImplication < ConverseImplication);
        assert!(MaterialNonImplication
            .partial_cmp(&MaterialImplication)
            .is_none());
        assert!(MaterialNonImplication < Disjunction);
        assert!(MaterialNonImplication < NotSecond::new());
        assert!(MaterialNonImplication
            .partial_cmp(&NotFirst::new())
            .is_none());
        assert!(MaterialNonImplication < ExclusiveDisjunction);
        assert!(MaterialNonImplication
            .partial_cmp(&LogicalBiconditional)
            .is_none());
        assert!(MaterialNonImplication < First {});
        assert!(MaterialNonImplication.partial_cmp(&Last {}).is_none());
        assert!(MaterialNonImplication
            .partial_cmp(&NonDisjunction)
            .is_none());
        assert_eq!(MaterialNonImplication, MaterialNonImplication);
        assert!(MaterialNonImplication
            .partial_cmp(&ConverseNonImplication)
            .is_none());
        assert!(MaterialNonImplication.partial_cmp(&Conjunction).is_none());
        assert!(MaterialNonImplication > Falsity);
    }

    #[test]
    fn converse_non_imply() {
        assert!(ConverseNonImplication < Truth);
        assert!(ConverseNonImplication < NonConjunction);
        assert!(ConverseNonImplication
            .partial_cmp(&ConverseImplication)
            .is_none());
        assert!(ConverseNonImplication < MaterialImplication);
        assert!(ConverseNonImplication < Disjunction);
        assert!(ConverseNonImplication
            .partial_cmp(&NotSecond::new())
            .is_none());
        assert!(ConverseNonImplication < NotFirst::new());
        assert!(ConverseNonImplication < ExclusiveDisjunction);
        assert!(ConverseNonImplication
            .partial_cmp(&LogicalBiconditional)
            .is_none());
        assert!(ConverseNonImplication.partial_cmp(&First {}).is_none());
        assert!(ConverseNonImplication < Last {});
        assert!(ConverseNonImplication
            .partial_cmp(&NonDisjunction)
            .is_none());
        assert!(ConverseNonImplication
            .partial_cmp(&MaterialNonImplication)
            .is_none());
        assert_eq!(ConverseNonImplication, ConverseNonImplication);
        assert!(ConverseNonImplication.partial_cmp(&Conjunction).is_none());
        assert!(ConverseNonImplication > Falsity);
    }

    #[test]
    fn conjunction() {
        assert!(Conjunction < Truth);
        assert!(Conjunction.partial_cmp(&NonConjunction).is_none());
        assert!(Conjunction < ConverseImplication);
        assert!(Conjunction < MaterialImplication);
        assert!(Conjunction < Disjunction);
        assert!(Conjunction.partial_cmp(&NotSecond::new()).is_none());
        assert!(Conjunction.partial_cmp(&NotFirst::new()).is_none());
        assert!(Conjunction.partial_cmp(&ExclusiveDisjunction).is_none());
        assert!(Conjunction < LogicalBiconditional);
        assert!(Conjunction < First {});
        assert!(Conjunction < Last {});
        assert!(Conjunction.partial_cmp(&NonDisjunction).is_none());
        assert!(Conjunction.partial_cmp(&MaterialNonImplication).is_none());
        assert!(Conjunction.partial_cmp(&ConverseNonImplication).is_none());
        assert_eq!(Conjunction, Conjunction);
        assert!(Conjunction > Falsity);
    }

    #[test]
    fn contradiction_is_lowest() {
        assert!(Falsity < Truth);
        assert!(Falsity < NonConjunction);
        assert!(Falsity < ConverseImplication);
        assert!(Falsity < MaterialImplication);
        assert!(Falsity < Disjunction);
        assert!(Falsity < NotSecond::new());
        assert!(Falsity < NotFirst::new());
        assert!(Falsity < ExclusiveDisjunction);
        assert!(Falsity < LogicalBiconditional);
        assert!(Falsity < First {});
        assert!(Falsity < Last {});
        assert!(Falsity < NonDisjunction);
        assert!(Falsity < MaterialNonImplication);
        assert!(Falsity < ConverseNonImplication);
        assert!(Falsity < Conjunction);
        assert_eq!(Falsity, Falsity);
    }
}
