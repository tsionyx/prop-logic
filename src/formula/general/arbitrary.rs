use proptest::prelude::*;

use super::formula::Formula;

#[derive(Debug, Clone)]
/// Parameters for generating [`Formula`]-s
/// with the [`Arbitrary`] trait.
pub struct Parameters<T> {
    /// How many levels of depth the generated formula can have.
    pub max_depth: u32,
    /// The atomic variables that can be used in the generated formula.
    pub atoms: Vec<T>,
}

impl<T> Default for Parameters<T> {
    fn default() -> Self {
        Self {
            max_depth: 8,
            atoms: vec![],
        }
    }
}

impl<T> Arbitrary for Formula<T>
where
    T: Clone + std::fmt::Debug + 'static,
{
    type Parameters = Parameters<T>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let Parameters { max_depth, atoms } = args;

        let leaf = prop_oneof![
            any::<bool>().prop_map(Self::truth),
            prop::sample::select(atoms).prop_map(Self::atom),
        ];

        let max_nodes = 1 << max_depth;
        leaf.prop_recursive(
            max_depth,
            max_nodes,
            2, // 2 child formulas max
            |inner| {
                prop_oneof![
                    1 => inner.clone().prop_map(|f| Self::not(f)),
                    2 => (inner.clone(), inner).prop_flat_map(|(a, b)| {
                        prop_oneof![
                            2 => Just(Self::and(a.clone(), b.clone())),
                            2 => Just(Self::or(a.clone(), b.clone())),
                            1 => Just(Self::xor(a.clone(), b.clone())),
                            1 => Just(Self::implies(a.clone(), b.clone())),
                            1 => Just(Self::equivalent(a, b)),
                        ]
                    }),
                ]
            },
        )
        .boxed()
    }
}
