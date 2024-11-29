use super::{functions, BoolFnExt};

use crate::{arity::two_powers_of_two_powers::D, utils::dependent_array::CheckedStorage};

/// This type stores all `BoolFnExt`-s for a given `ARITY`.
///
/// Also, it allows to compare the given generic function
/// (identified by their truth tables, TO BE DONE) with the [concrete ones][BoolFnExt].
pub type AllFunctions<const ARITY: usize> = CheckedStorage<ARITY, D, &'static dyn BoolFnExt<ARITY>>;

const _ASSERT_0: () = <AllFunctions<0>>::ASSERT_SIZE;
const _ASSERT_1: () = <AllFunctions<1>>::ASSERT_SIZE;
const _ASSERT_2: () = <AllFunctions<2>>::ASSERT_SIZE;

#[allow(trivial_casts)] // need to define at least one cast to prevent compile-error
/// The array of 0-arity functions.
pub const NULLARY_FUNCTIONS: AllFunctions<0> = CheckedStorage::new([
    &functions::Falsity as &'static dyn BoolFnExt<0>,
    &functions::Truth,
]);

#[allow(trivial_casts)] // need to define at least one cast to prevent compile-error
/// The array of unary functions.
pub const UNARY_FUNCTIONS: AllFunctions<1> = CheckedStorage::new([
    &functions::Falsity as &'static dyn BoolFnExt<1>,
    &functions::LogicalIdentity,
    &functions::Negation,
    &functions::Truth,
]);

type NProj0 = functions::ProjectAndUnary<0, functions::Negation>;
type NProj1 = functions::ProjectAndUnary<1, functions::Negation>;

#[allow(trivial_casts)] // need to define at least one cast to prevent compile-error
/// The array of unary functions.
pub const BINARY_FUNCTIONS: AllFunctions<2> = CheckedStorage::new([
    &functions::Falsity as &'static dyn BoolFnExt<2>, // 0 0 0 0
    &functions::Conjunction,                          // 0 0 0 1
    &functions::MaterialNonImplication,               // 0 0 1 0
    &functions::Projection::<0>,                      // 0 0 1 1
    &functions::ConverseNonImplication,               // 0 1 0 0
    &functions::Projection::<1>,                      // 0 1 0 1
    &functions::ExclusiveDisjunction,                 // 0 1 1 0
    &functions::Disjunction,                          // 0 1 1 1
    &functions::NonDisjunction,                       // 1 0 0 0
    &functions::LogicalBiconditional,                 // 1 0 0 1
    &NProj1::new(),                                   // 1 0 1 0
    &functions::ConverseImplication,                  // 1 0 1 1
    &NProj0::new(),                                   // 1 1 0 0
    &functions::MaterialImplication,                  // 1 1 0 1
    &functions::NonConjunction,                       // 1 1 1 0
    &functions::Truth,                                // 1 1 1 1
]);

#[cfg(test)]
mod tests_ordering {
    use super::*;

    #[test]
    fn nullary() {
        for (i, f1) in NULLARY_FUNCTIONS.iter().enumerate() {
            let t1: Vec<_> = f1.get_truth_table().into_iter().map(|(_, k)| k).collect();
            dbg!(&t1);
            for f2 in NULLARY_FUNCTIONS.iter().skip(i + 1) {
                let t2: Vec<_> = f2.get_truth_table().into_iter().map(|(_, k)| k).collect();
                assert!(t1 < t2, "{t1:?} >= {t2:?}");
            }
        }
    }

    #[test]
    fn unary() {
        for (i, f1) in UNARY_FUNCTIONS.iter().enumerate() {
            let t1: Vec<_> = f1.get_truth_table().into_iter().map(|(_, k)| k).collect();
            dbg!(&t1);
            for f2 in UNARY_FUNCTIONS.iter().skip(i + 1) {
                let t2: Vec<_> = f2.get_truth_table().into_iter().map(|(_, k)| k).collect();
                assert!(t1 < t2, "{t1:?} >= {t2:?}");
            }
        }
    }

    #[test]
    fn binary() {
        for (i, f) in BINARY_FUNCTIONS.iter().enumerate() {
            let t: Vec<_> = f.get_truth_table().into_iter().map(|(_, k)| k).collect();
            dbg!(i, t);
        }

        for (i, f1) in BINARY_FUNCTIONS.iter().enumerate() {
            let t1: Vec<_> = f1.get_truth_table().into_iter().map(|(_, k)| k).collect();
            dbg!(i, &t1);
            for f2 in BINARY_FUNCTIONS.iter().skip(i + 1) {
                let t2: Vec<_> = f2.get_truth_table().into_iter().map(|(_, k)| k).collect();
                assert!(t1 < t2, "{i} {t1:?} >= {t2:?}");
            }
        }
    }
}
