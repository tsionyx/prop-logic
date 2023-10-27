use std::cmp::Ordering;

use super::{
    and::Conjunction,
    converse_imply::ConverseImplication,
    converse_nimply::ConverseNonImplication,
    falsity::Falsity,
    id::LogicalIdentity,
    imply::MaterialImplication,
    nand::NonConjunction,
    neg::Negation,
    nimply::MaterialNonImplication,
    nor::NonDisjunction,
    or::Disjunction,
    proj::{ProjectAndUnary, Projection},
    truth::Truth,
    truth_table,
    xnor::LogicalBiconditional,
    xor::ExclusiveDisjunction,
    TruthFunction,
};

use crate::utils::cartesian_diag;

/// Defines the partial order (`<=`) between the [`TruthFunction`]-s
/// by associating them with the sets.
///
/// For more information, see the
/// [Haase diagram](https://en.wikipedia.org/wiki/Logical_connective#Table_and_Hasse_diagram).
fn partial_ordering<Op1, Op2, const ARITY: usize>() -> Option<Ordering>
where
    Op1: TruthFunction<ARITY>,
    Op2: TruthFunction<ARITY>,
{
    let op1_truth_table = truth_table::get::<Op1, ARITY>();
    let op2_truth_table = truth_table::get::<Op2, ARITY>();

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

type Proj0 = Projection<0>;
type Proj1 = Projection<1>;
type NProj0 = ProjectAndUnary<0, Negation>;
type NProj1 = ProjectAndUnary<1, Negation>;

macro_rules! ignore {
    ($a: tt, $b: tt) => {};
}

cartesian_diag!(
    square
    [
        Truth,
        Falsity,
        Proj0,
        Proj1,
        NProj0,
        NProj1,
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
        assert!(LogicalIdentity.partial_cmp(&Negation).is_none());
        assert!(LogicalIdentity < Truth);
    }

    #[test]
    fn unary_negation() {
        assert!(Negation > Falsity);
        assert!(Negation.partial_cmp(&LogicalIdentity).is_none());
        assert!(Negation < Truth);
    }

    #[test]
    fn contradiction_is_lowest() {
        assert!(Falsity < Truth);
        assert!(Falsity < LogicalIdentity);
        assert!(Falsity < Negation);
        assert!(Falsity < Projection::<0>);
        assert!(Falsity < Projection::<1>);
        assert!(Falsity < ProjectAndUnary::<0, Negation>::new());
        assert!(Falsity < ProjectAndUnary::<1, Negation>::new());
        // TODO: 10 more
    }

    #[test]
    fn tautology_is_greatest() {
        assert!(Truth > Falsity);
        assert!(Truth > LogicalIdentity);
        assert!(Truth > Negation);
        assert!(Truth > Projection::<0>);
        assert!(Truth > Projection::<1>);
        assert!(Truth > ProjectAndUnary::<0, Negation>::new());
        assert!(Truth > ProjectAndUnary::<1, Negation>::new());
        // TODO: 10 more
    }

    #[test]
    fn left_projection() {
        assert!(Projection::<0> < Truth);
        assert!(Projection::<0> > Falsity);
        assert!(Projection::<0>.partial_cmp(&Projection::<1>).is_none());
        assert!(Projection::<0>
            .partial_cmp(&ProjectAndUnary::<0, Negation>::new())
            .is_none());
        assert!(Projection::<0>
            .partial_cmp(&ProjectAndUnary::<1, Negation>::new())
            .is_none());
        // TODO: 10 more
    }

    #[test]
    fn right_projection() {
        assert!(Projection::<1> < Truth);
        assert!(Projection::<1> > Falsity);
        assert!(Projection::<1>.partial_cmp(&Projection::<0>).is_none());
        assert!(Projection::<1>
            .partial_cmp(&ProjectAndUnary::<0, Negation>::new())
            .is_none());
        assert!(Projection::<1>
            .partial_cmp(&ProjectAndUnary::<1, Negation>::new())
            .is_none());
        // TODO: 10 more
    }

    #[test]
    fn neg_left_projection() {
        assert!(ProjectAndUnary::<0, Negation>::new() < Truth);
        assert!(ProjectAndUnary::<0, Negation>::new() > Falsity);
        assert!(ProjectAndUnary::<0, Negation>::new()
            .partial_cmp(&Projection::<0>)
            .is_none());
        assert!(ProjectAndUnary::<0, Negation>::new()
            .partial_cmp(&Projection::<1>)
            .is_none());
        assert!(ProjectAndUnary::<0, Negation>::new()
            .partial_cmp(&ProjectAndUnary::<1, Negation>::new())
            .is_none());
        // TODO: 10 more
    }

    #[test]
    fn neg_right_projection() {
        assert!(ProjectAndUnary::<1, Negation>::new() < Truth);
        assert!(ProjectAndUnary::<1, Negation>::new() > Falsity);
        assert!(ProjectAndUnary::<1, Negation>::new()
            .partial_cmp(&Projection::<0>)
            .is_none());
        assert!(ProjectAndUnary::<1, Negation>::new()
            .partial_cmp(&Projection::<1>)
            .is_none());
        assert!(ProjectAndUnary::<1, Negation>::new()
            .partial_cmp(&ProjectAndUnary::<0, Negation>::new())
            .is_none());
        // TODO: 10 more
    }

    // TODO: 10 more tests for every connective
}
