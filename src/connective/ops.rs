//! Unary operations on the [`TruthFunction`]-s.
use std::ops::Not;

use super::{functions::*, TruthFunction};

/// Easily convert a `TruthFunction` into its counterpart in terms
/// of switching all the bits in its truth table.
pub trait Negate<const ARITY: usize>: TruthFunction<ARITY> {
    /// Another `TruthFunction` which truth table is a negation of the original one.
    type Not: TruthFunction<ARITY>;
}

macro_rules! impl_negate {
    ($arity:literal: $($t:ty =! $neg_t:ty),+ $(,)?) => {
        $(
            impl Negate<$arity> for $t
            {
                type Not = $neg_t;
            }

            impl Negate<$arity> for $neg_t
            {
                type Not = $t;
            }
        )+
    };
}

macro_rules! impl_std_not {
    ($arity:literal: $($t:ty),+ $(,)?) => {
        $(
            impl Not for $t
            {
                type Output = <$t as Negate<$arity>>::Not;

                fn not(self) -> Self::Output {
                    <$t as Negate<$arity>>::Not::init()
                }
            }
        )+
    };
}

/// Easily convert a binary `TruthFunction` into its counterpart in terms
/// of swapping its arguments.
///
/// For the _commutative_ operation, the [`Conversion`] preserves the function.
pub trait Converse: TruthFunction<2> {
    /// Another `TruthFunction` which truth function is an conversion of the original one.
    type Conversion: TruthFunction<2>;
}

macro_rules! impl_converse {
    ($($t:ty =! $con_t:ty),+ $(,)?) => {
        $(
            impl Converse for $t
            {
                type Conversion = $con_t;
            }

            impl Converse for $con_t
            {
                type Conversion = $t;
            }
        )+
    };

    // commutative case
    ($($t:ty),+ $(,)?) => {
        $(
            impl Converse for $t
            {
                type Conversion = Self;
            }
        )+
    };
}

// TODO: impl std::ops::Neg? (could be confusing)

// implementations for nullary and unary
impl<const ARITY: usize> Negate<ARITY> for Falsity {
    type Not = Truth;
}

impl Not for Falsity {
    type Output = Truth;

    fn not(self) -> Self::Output {
        Truth
    }
}

impl_negate![1: LogicalIdentity =! Negation];
impl_std_not![1: LogicalIdentity, Negation];

impl<const ARITY: usize> Negate<ARITY> for Truth {
    type Not = Falsity;
}

impl Not for Truth {
    type Output = Falsity;

    fn not(self) -> Self::Output {
        Falsity
    }
}

// implementations for binary
impl_negate![2:
    Conjunction =! NonConjunction,
    MaterialNonImplication =! MaterialImplication,
    Projection::<0> =! ProjectAndUnary::<0, Negation>,
    ConverseNonImplication =! ConverseImplication,
    Projection::<1> =! ProjectAndUnary::<1, Negation>,
    ExclusiveDisjunction =! LogicalBiconditional,
    Disjunction =! NonDisjunction,
];
impl_std_not![2:
    Conjunction, NonConjunction,
    MaterialNonImplication, MaterialImplication,
    Projection::<0>, ProjectAndUnary::<0, Negation>,
    ConverseNonImplication, ConverseImplication,
    Projection::<1>, ProjectAndUnary::<1, Negation>,
    ExclusiveDisjunction, LogicalBiconditional,
    Disjunction, NonDisjunction,
];

impl_converse![
    Falsity,
    Conjunction,
    ExclusiveDisjunction,
    Disjunction,
    NonDisjunction,
    LogicalBiconditional,
    NonConjunction,
    Truth
];
impl_converse![
    MaterialNonImplication = !ConverseNonImplication,
    Projection::<0> = !Projection::<1>,
    MaterialImplication = !ConverseImplication,
    ProjectAndUnary::<0, Negation> = !ProjectAndUnary::<1, Negation>,
];

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{arity::two_powers, utils::dependent_array::CheckedArray};

    use super::*;

    fn assert_neg<const ARITY: usize, N: Negate<ARITY>>()
    where
        two_powers::D: CheckedArray<ARITY>,
    {
        let table = N::init().get_truth_table().into_values();
        let table_neg = N::Not::init().get_truth_table().into_values();

        dbg!(std::any::type_name::<N>());
        dbg!(std::any::type_name::<N::Not>());
        for (x, y) in table.into_iter().zip(table_neg) {
            assert_eq!(x, !y);
        }
    }

    fn assert_std_not<const ARITY: usize, N, N2>(x: N)
    where
        N: Not<Output = N2> + TruthFunction<ARITY>,
        N2: TruthFunction<ARITY>,
        two_powers::D: CheckedArray<ARITY>,
    {
        let table = x.get_truth_table().into_values();
        let table_neg = (!x).get_truth_table().into_values();

        dbg!(std::any::type_name::<N>());
        for (x, y) in table.into_iter().zip(table_neg) {
            assert_eq!(x, !y);
        }
    }

    fn assert_conversion<C: Converse>() {
        let table = C::init().get_truth_table().into_inner();
        let table_conversed: HashMap<_, _> = C::Conversion::init()
            .get_truth_table()
            .into_inner()
            .into_iter()
            .collect();
        assert_eq!(table_conversed.len(), 4);

        dbg!(std::any::type_name::<C>());
        dbg!(std::any::type_name::<C::Conversion>());
        for ([x, y], val) in table {
            let conversed_args = [y, x];
            let conversed_val = *table_conversed
                .get(&conversed_args)
                .expect("Should contain all combinations");
            assert_eq!(val, conversed_val);
        }
    }

    #[test]
    fn test_all_negations() {
        // nullary
        assert_neg::<0, Falsity>();
        assert_std_not::<0, _, _>(Falsity);

        assert_neg::<0, Truth>();
        assert_std_not::<0, _, _>(Truth);

        // unary
        assert_neg::<1, Falsity>();
        assert_std_not::<1, _, _>(Falsity);

        assert_neg::<1, LogicalIdentity>();
        assert_std_not::<1, _, _>(LogicalIdentity);

        assert_neg::<1, Negation>();
        assert_std_not::<1, _, _>(Negation);

        assert_neg::<1, Truth>();
        assert_std_not::<1, _, _>(Truth);

        // binary
        assert_neg::<2, Falsity>();
        assert_std_not::<2, _, _>(Falsity);

        assert_neg::<2, Conjunction>();
        assert_std_not::<2, _, _>(Conjunction);

        assert_neg::<2, MaterialNonImplication>();
        assert_std_not::<2, _, _>(MaterialNonImplication);

        assert_neg::<2, Projection<0>>();
        assert_std_not::<2, _, _>(Projection::<0>);

        assert_neg::<2, ConverseNonImplication>();
        assert_std_not::<2, _, _>(ConverseNonImplication);

        assert_neg::<2, Projection<1>>();
        assert_std_not::<2, _, _>(Projection::<1>);

        assert_neg::<2, ExclusiveDisjunction>();
        assert_std_not::<2, _, _>(ExclusiveDisjunction);

        assert_neg::<2, Disjunction>();
        assert_std_not::<2, _, _>(Disjunction);

        assert_neg::<2, NonDisjunction>();
        assert_std_not::<2, _, _>(NonDisjunction);

        assert_neg::<2, LogicalBiconditional>();
        assert_std_not::<2, _, _>(LogicalBiconditional);

        assert_neg::<2, ProjectAndUnary<1, Negation>>();
        assert_std_not::<2, _, _>(ProjectAndUnary::<1, Negation>::new());

        assert_neg::<2, ConverseImplication>();
        assert_std_not::<2, _, _>(ConverseImplication);

        assert_neg::<2, ProjectAndUnary<0, Negation>>();
        assert_std_not::<2, _, _>(ProjectAndUnary::<0, Negation>::new());

        assert_neg::<2, MaterialImplication>();
        assert_std_not::<2, _, _>(MaterialImplication);

        assert_neg::<2, NonConjunction>();
        assert_std_not::<2, _, _>(NonConjunction);

        assert_neg::<2, Truth>();
        assert_std_not::<2, _, _>(Truth);
    }

    #[test]
    fn test_all_conversions() {
        assert_conversion::<Falsity>();
        assert_conversion::<Conjunction>();
        assert_conversion::<MaterialNonImplication>();
        assert_conversion::<Projection<0>>();
        assert_conversion::<ConverseNonImplication>();
        assert_conversion::<Projection<1>>();
        assert_conversion::<ExclusiveDisjunction>();
        assert_conversion::<Disjunction>();
        assert_conversion::<NonDisjunction>();
        assert_conversion::<LogicalBiconditional>();
        assert_conversion::<ProjectAndUnary<1, Negation>>();
        assert_conversion::<ConverseImplication>();
        assert_conversion::<ProjectAndUnary<0, Negation>>();
        assert_conversion::<MaterialImplication>();
        assert_conversion::<NonConjunction>();
        assert_conversion::<Truth>();
    }
}
