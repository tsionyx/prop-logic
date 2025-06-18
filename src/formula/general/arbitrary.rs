use std::fmt::Debug;

use proptest::prelude::*;

use super::{
    super::ops::{And as _, Equivalent as _, Implies as _, Not as _, Or as _, Xor as _},
    formula::Formula,
};

#[derive(Debug, Clone)]
/// Parameters for generating [`Formula`]-s
/// with the [`Arbitrary`] trait.
pub struct Parameters<T> {
    /// The relative weight of the [constants][Formula::TruthValue]
    /// in the leaves of the generated [`Formula`].
    ///
    /// By default it is set to 2 to uniformly select
    /// one of constant along with one of the variables provided.
    ///
    /// Set it to 0 to prevent using boolean constants.
    pub leaf_const_weight: u32,

    /// The relative weight of the [atoms][Formula::Atomic]
    /// in the leaves of the generated [`Formula`].
    ///
    /// By default it is set to `None` in which case
    /// it unwraps into `self.variables.len()` to uniformly select
    /// one of constant along with one of the variables provided.
    ///
    /// Set it to 0 to prevent using variables.
    pub leaf_var_weight: Option<u32>,

    /// The variables that can be used in the generated [`Formula`].
    pub variables: Vec<T>,

    /// The relative weights of the unary to binary operators
    /// in the recursive case for the generated [`Formula`].
    ///
    /// By default it is set to 1:2.
    ///
    /// Setting the first to 0 prevents using unary operations (primarily [`crate::formula::Not`]).
    /// Setting the second to 0 prevents using binary operations.
    pub unary_to_binary_ratio: (u32, u32),

    /// The relative weights of the binary operations generated.
    pub binary_weights: BinaryWeights,

    /// How many levels of depth the generated [`Formula`] can have.
    pub max_depth: u32,

    /// Whether to use [dynamic connectives][Formula::Dynamic] or not.
    pub use_dynamic: bool,
}

impl<T> Default for Parameters<T> {
    fn default() -> Self {
        Self {
            leaf_const_weight: 2,
            leaf_var_weight: None,
            variables: vec![],
            unary_to_binary_ratio: (1, 2),
            binary_weights: BinaryWeights::default(),
            max_depth: 8,
            use_dynamic: false,
        }
    }
}

#[derive(Debug, Copy, Clone)]
/// Parameters for generating binary [`Formula`]-s
/// with the [`Arbitrary`] trait.
pub struct BinaryWeights {
    /// The relative weight of the [conjunction][Formula::And].
    ///
    /// By default it is set to 2.
    ///
    /// Setting it to 0 prevents generating conjunctions.
    pub conjunction: u32,

    /// The relative weight of the [disjunction][Formula::Or].
    ///
    /// By default it is set to 2.
    ///
    /// Setting it to 0 prevents generating disjunctions.
    pub disjunction: u32,

    /// The relative weight of the [exclusive disjunction][Formula::Xor].
    ///
    /// By default it is set to 1.
    ///
    /// Setting it to 0 prevents generating exclusive disjunctions.
    pub xor_disjunction: u32,

    /// The relative weight of the [implication][Formula::Implies].
    ///
    /// By default it is set to 1.
    ///
    /// Setting it to 0 prevents generating implications.
    pub implication: u32,

    /// The relative weight of the [equivalence][Formula::Equivalent].
    ///
    /// By default it is set to 1.
    ///
    /// Setting it to 0 prevents generating equivalences.
    pub equivalence: u32,
}

impl Default for BinaryWeights {
    fn default() -> Self {
        Self {
            conjunction: 2,
            disjunction: 2,
            xor_disjunction: 1,
            implication: 1,
            equivalence: 1,
        }
    }
}

impl<T> Formula<T>
where
    T: Debug + Clone + 'static,
{
    fn generate_nullary(use_dynamic: bool) -> BoxedStrategy<Self> {
        let static_consts = any::<bool>().prop_map(Self::truth);

        if use_dynamic {
            use crate::connective::{Falsity, Truth};

            let dynamic = vec![Self::nullary(Truth), Self::nullary(Falsity)];
            let dynamic_consts = prop::sample::select(dynamic);
            prop_oneof![static_consts, dynamic_consts].boxed()
        } else {
            static_consts.boxed()
        }
    }

    fn generate_unary(input: BoxedStrategy<Self>, use_dynamic: bool) -> BoxedStrategy<Self> {
        let static_unary = input
            .clone()
            .prop_flat_map(|f| prop::sample::select(vec![f.clone(), Self::not(f)]));

        if use_dynamic {
            use crate::connective::{LogicalIdentity, Negation};

            let dynamic_unary = input.prop_flat_map(|f| {
                let dynamic = vec![
                    Self::unary(LogicalIdentity, f.clone()),
                    Self::unary(Negation, f),
                ];
                prop::sample::select(dynamic)
            });

            prop_oneof![static_unary, dynamic_unary].boxed()
        } else {
            static_unary.boxed()
        }
    }

    fn generate_binary(
        input: BoxedStrategy<Self>,
        weights: BinaryWeights,
        use_dynamic: bool,
    ) -> BoxedStrategy<Self> {
        let BinaryWeights {
            conjunction,
            disjunction,
            xor_disjunction,
            implication,
            equivalence,
        } = weights;
        let static_binary = (input.clone(), input.clone()).prop_flat_map(move |(f1, f2)| {
            prop_oneof![
                conjunction => Just(Self::and(f1.clone(), f2.clone())),
                disjunction => Just(Self::or(f1.clone(), f2.clone())),
                xor_disjunction => Just(Self::xor(f1.clone(), f2.clone())),
                implication => Just(Self::implies(f1.clone(), f2.clone())),
                equivalence => Just(Self::equivalent(f1, f2)),
            ]
        });

        if use_dynamic {
            use crate::connective::{
                Conjunction, Disjunction, ExclusiveDisjunction, LogicalBiconditional,
                MaterialImplication, NonConjunction, NonDisjunction,
            };

            let dynamic_binary = (input.clone(), input).prop_flat_map(|(f1, f2)| {
                let dynamic = vec![
                    Self::binary(Conjunction, f1.clone(), f2.clone()),
                    // Self::binary(MaterialNonImplication, f1.clone(), f2.clone()),
                    // Self::binary(First, f1.clone(), f2.clone()),
                    // Self::binary(ConverseNonImplication, f1.clone(), f2.clone()),
                    // Self::binary(Last, f1.clone(), f2.clone()),
                    Self::binary(ExclusiveDisjunction, f1.clone(), f2.clone()),
                    Self::binary(Disjunction, f1.clone(), f2.clone()),
                    Self::binary(NonDisjunction, f1.clone(), f2.clone()),
                    Self::binary(LogicalBiconditional, f1.clone(), f2.clone()),
                    // Self::binary(NotSecond, f1.clone(), f2.clone()),
                    // Self::binary(ConverseImplication, f1.clone(), f2.clone()),
                    // Self::binary(NotFirst, f1.clone(), f2.clone()),
                    Self::binary(MaterialImplication, f1.clone(), f2.clone()),
                    Self::binary(NonConjunction, f1, f2),
                ];
                prop::sample::select(dynamic)
            });

            prop_oneof![static_binary, dynamic_binary].boxed()
        } else {
            static_binary.boxed()
        }
    }
}

impl<T> Arbitrary for Formula<T>
where
    T: Debug + Clone + 'static,
{
    type Parameters = Parameters<T>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let Parameters {
            leaf_const_weight,
            leaf_var_weight,
            variables,
            unary_to_binary_ratio: (unary_weight, binary_weight),
            binary_weights,
            max_depth,
            use_dynamic,
        } = args;

        let typed_consts = Self::generate_nullary(use_dynamic);

        let leaf_var_weight =
            leaf_var_weight.unwrap_or_else(|| u32::try_from(variables.len()).unwrap_or(u32::MAX));
        let leaf = prop_oneof![
            leaf_const_weight => typed_consts,
            leaf_var_weight => prop::sample::select(variables).prop_map(Self::atom),
        ];

        let max_nodes = 1 << max_depth;
        leaf.prop_recursive(
            max_depth,
            max_nodes,
            2, // 2 child formulas max
            move |inner| {
                prop_oneof![
                    unary_weight => Self::generate_unary(inner.clone(), use_dynamic),
                    binary_weight => Self::generate_binary(inner, binary_weights, use_dynamic),
                ]
            },
        )
        .boxed()
    }
}
