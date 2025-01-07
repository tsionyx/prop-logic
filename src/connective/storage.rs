use super::{functions, BoolFnExt, Connective};

use crate::{
    arity::two_powers_of_two_powers::D,
    utils::{dependent_array::CheckedStorage, upcast::Upcast},
};

/// Combination of [`BoolFnExt`] and [`Connective`] to allow to use in dyn context.
///
/// Implemented automatically for all `F: BoolFnExt + Connective`.
pub trait StoredBoolFn<const ARITY: usize>:
    BoolFnExt<ARITY> + Connective<ARITY> + Upcast<dyn BoolFnExt<ARITY>>
{
}

impl<const ARITY: usize, F> StoredBoolFn<ARITY> for F where
    F: BoolFnExt<ARITY> + Connective<ARITY> + Upcast<dyn BoolFnExt<ARITY>>
{
}

/// This type stores all [`StoredBoolFn`]-s for a given `ARITY`.
///
/// Also, it allows to compare the given generic function
/// (identified by their truth tables, TO BE DONE) with the [concrete ones][BoolFnExt].
pub type AllFunctions<const ARITY: usize> =
    CheckedStorage<ARITY, D, &'static dyn StoredBoolFn<ARITY>>;

const _ASSERT_0: () = <AllFunctions<0>>::ASSERT_SIZE;
const _ASSERT_1: () = <AllFunctions<1>>::ASSERT_SIZE;
const _ASSERT_2: () = <AllFunctions<2>>::ASSERT_SIZE;

#[allow(trivial_casts)] // need to define at least one cast to prevent compile-error
/// The array of all possible 0-arity functions up to equivalence.
pub const NULLARY_FUNCTIONS: AllFunctions<0> = CheckedStorage::new([
    &functions::Falsity as &'static dyn StoredBoolFn<0>,
    &functions::Truth,
]);

#[allow(trivial_casts)] // need to define at least one cast to prevent compile-error
/// The array of all possible unary functions up to equivalence.
pub const UNARY_FUNCTIONS: AllFunctions<1> = CheckedStorage::new([
    &functions::Falsity as &'static dyn StoredBoolFn<1>,
    &functions::LogicalIdentity,
    &functions::Negation,
    &functions::Truth,
]);

#[allow(trivial_casts)] // need to define at least one cast to prevent compile-error
/// The array of all possible binary functions up to equivalence.
pub const BINARY_FUNCTIONS: AllFunctions<2> = CheckedStorage::new([
    &functions::Falsity as &'static dyn StoredBoolFn<2>, // 0 0 0 0
    &functions::Conjunction,                             // 0 0 0 1
    &functions::MaterialNonImplication,                  // 0 0 1 0
    &functions::First {},                                // 0 0 1 1
    &functions::ConverseNonImplication,                  // 0 1 0 0
    &functions::Last {},                                 // 0 1 0 1
    &functions::ExclusiveDisjunction,                    // 0 1 1 0
    &functions::Disjunction,                             // 0 1 1 1
    &functions::NonDisjunction,                          // 1 0 0 0
    &functions::LogicalBiconditional,                    // 1 0 0 1
    &functions::NotSecond::new(),                        // 1 0 1 0
    &functions::ConverseImplication,                     // 1 0 1 1
    &functions::NotFirst::new(),                         // 1 1 0 0
    &functions::MaterialImplication,                     // 1 1 0 1
    &functions::NonConjunction,                          // 1 1 1 0
    &functions::Truth,                                   // 1 1 1 1
]);

#[cfg(test)]
mod tests_ordering {
    use super::*;

    #[test]
    fn nullary() {
        for (i, f1) in NULLARY_FUNCTIONS.as_ref().iter().enumerate() {
            let t1: Vec<_> = f1.get_truth_table().into_iter().map(|(_, k)| k).collect();
            dbg!(&t1);
            for f2 in NULLARY_FUNCTIONS.as_ref().iter().skip(i + 1) {
                let t2: Vec<_> = f2.get_truth_table().into_iter().map(|(_, k)| k).collect();
                assert!(t1 < t2, "{t1:?} >= {t2:?}");
            }
        }
    }

    #[test]
    fn unary() {
        for (i, f1) in UNARY_FUNCTIONS.as_ref().iter().enumerate() {
            let t1: Vec<_> = f1.get_truth_table().into_iter().map(|(_, k)| k).collect();
            dbg!(&t1);
            for f2 in UNARY_FUNCTIONS.as_ref().iter().skip(i + 1) {
                let t2: Vec<_> = f2.get_truth_table().into_iter().map(|(_, k)| k).collect();
                assert!(t1 < t2, "{t1:?} >= {t2:?}");
            }
        }
    }

    #[test]
    fn binary() {
        for (i, f) in BINARY_FUNCTIONS.as_ref().iter().enumerate() {
            let t: Vec<_> = f.get_truth_table().into_iter().map(|(_, k)| k).collect();
            dbg!(i, t);
        }

        for (i, f1) in BINARY_FUNCTIONS.as_ref().iter().enumerate() {
            let t1: Vec<_> = f1.get_truth_table().into_iter().map(|(_, k)| k).collect();
            dbg!(i, &t1);
            for f2 in BINARY_FUNCTIONS.as_ref().iter().skip(i + 1) {
                let t2: Vec<_> = f2.get_truth_table().into_iter().map(|(_, k)| k).collect();
                assert!(t1 < t2, "{i} {t1:?} >= {t2:?}");
            }
        }
    }
}
