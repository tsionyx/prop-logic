use proptest::prelude::*;

use crate::{formula::ops::Equivalent as _, TruthTabled};

use super::{
    super::{super::FormulaParameters, formula::Formula},
    examples::reduce_all_with_basic_rules,
    *,
};

fn params() -> FormulaParameters<char> {
    FormulaParameters {
        variables: vec!['a', 'b', 'c', 'd'],
        leaf_var_weight: Some(10),
        use_dynamic: true,
        ..FormulaParameters::default()
    }
}

mod reduce_on_err_does_not_change_formula {
    use super::*;

    proptest! {
        // https://proptest-rs.github.io/proptest/proptest/tutorial/config.html
        #![proptest_config(ProptestConfig::with_cases(1000))]

        #[test]
        fn eliminate(f in Formula::arbitrary_with(params())) {
            if let Err(f2) = constant::EliminateConstants.transform(f.clone()) {
                assert_eq!(f, f2);
            }
        }

        #[test]
        fn canonical(f in Formula::arbitrary_with(params())) {
            if let Err(f2) = canonical::NoDynamicConnective.transform(f.clone()) {
                assert_eq!(f, f2);
            }
        }

        #[test]
        fn double_negation(f in Formula::arbitrary_with(params())) {
            if let Err(f2) = neg::DoubleNegation.transform(f.clone()) {
                assert_eq!(f, f2);
            }
        }

        #[test]
        fn sort_operators(f in Formula::arbitrary_with(params())) {
            if let Err(f2) = sort::SortAssociativeOperators.transform(f.clone()) {
                assert_eq!(f, f2);
            }
        }

        #[test]
        fn idempotence(f in Formula::arbitrary_with(params())) {
            if let Err(f2) = eq::Idempotence.transform(f.clone()) {
                assert_eq!(f, f2);
            }
        }

        #[test]
        fn negation(f in Formula::arbitrary_with(params())) {
            if let Err(f2) = eq::Negation.transform(f.clone()) {
                assert_eq!(f, f2);
            }
        }

        #[test]
        fn absorption(f in Formula::arbitrary_with(params())) {
            if let Err(f2) = absorption::Absorption.transform(f.clone()) {
                assert_eq!(f, f2);
            }
        }

        #[test]
        fn absorption_neg(f in Formula::arbitrary_with(params())) {
            if let Err(f2) = absorption::AbsorptionWithNeg.transform(f.clone()) {
                assert_eq!(f, f2);
            }
        }

        #[test]
        fn de_morgan(f in Formula::arbitrary_with(params())) {
            if let Err(f2) = elimination::DeMorgan.transform(f.clone()) {
                assert_eq!(f, f2);
            }
        }

        #[test]
        fn xor_equiv_neg(f in Formula::arbitrary_with(params())) {
            if let Err(f2) = elimination::XorEquivNegation.transform(f.clone()) {
                assert_eq!(f, f2);
            }
        }

        #[test]
        fn xor_eliminate(f in Formula::arbitrary_with(params())) {
            if let Err(f2) = elimination::Xor.transform(f.clone()) {
                assert_eq!(f, f2);
            }
        }

        #[test]
        fn equiv_eliminate(f in Formula::arbitrary_with(params())) {
            if let Err(f2) = elimination::Equiv.transform(f.clone()) {
                assert_eq!(f, f2);
            }
        }

        #[test]
        fn implication(f in Formula::arbitrary_with(params())) {
            if let Err(f2) = elimination::Implication.transform(f.clone()) {
                assert_eq!(f, f2);
            }
        }

        #[test]
        fn distrib_conjunction(f in Formula::arbitrary_with(params())) {
            if let Err(f2) = distrib::DistributeConjunctionOverDisjunction.transform(f.clone()) {
                assert_eq!(f, f2);
            }
        }

        #[test]
        fn distrib_disjunction(f in Formula::arbitrary_with(params())) {
            if let Err(f2) = distrib::DistributeDisjunctionOverConjunction.transform(f.clone()) {
                assert_eq!(f, f2);
            }
        }
    }
}

mod apply_all_does_not_change_formula {
    use std::{fmt::Display, hash::Hash};

    use super::*;

    fn assert_equivalent_after_transform<T, R>(f: Formula<T>, rule: &R)
    where
        T: Clone + Eq + Hash + Display + 'static,
        R: RewritingRule<T>,
    {
        let res = rule.apply_all(f.clone(), true);
        assert!(
            f.is_equivalent(&res),
            "Expected {f} to be equivalent to {res}"
        );

        let f = f.equivalent(res);
        assert!(
            f.is_equivalent(&Formula::truth(true)),
            "Expected {f} to be tautology"
        );
    }

    proptest! {
        // https://proptest-rs.github.io/proptest/proptest/tutorial/config.html
        #![proptest_config(ProptestConfig::with_cases(1000))]

        #[test]
        fn eliminate(f in Formula::arbitrary_with(params())) {
            assert_equivalent_after_transform(f, &constant::EliminateConstants);
        }

        #[test]
        fn canonical(f in Formula::arbitrary_with(params())) {
            assert_equivalent_after_transform(f, &canonical::NoDynamicConnective);
        }

        #[test]
        fn double_negation(f in Formula::arbitrary_with(params())) {
            assert_equivalent_after_transform(f, &neg::DoubleNegation);
        }

        #[test]
        fn sort_operators(f in Formula::arbitrary_with(params())) {
            assert_equivalent_after_transform(f, &sort::SortAssociativeOperators);
        }

        #[test]
        fn idempotence(f in Formula::arbitrary_with(params())) {
            assert_equivalent_after_transform(f, &eq::Idempotence);
        }

        #[test]
        fn negation(f in Formula::arbitrary_with(params())) {
            assert_equivalent_after_transform(f, &eq::Negation);
        }

        #[test]
        fn absorption(f in Formula::arbitrary_with(params())) {
            assert_equivalent_after_transform(f, &absorption::Absorption);
        }

        #[test]
        fn absorption_neg(f in Formula::arbitrary_with(params())) {
            assert_equivalent_after_transform(f, &absorption::AbsorptionWithNeg);
        }

        #[test]
        fn de_morgan(f in Formula::arbitrary_with(params())) {
            assert_equivalent_after_transform(f, &elimination::DeMorgan);
        }

        #[test]
        fn xor_equiv_neg(f in Formula::arbitrary_with(params())) {
            assert_equivalent_after_transform(f, &elimination::XorEquivNegation);
        }

        #[test]
        fn xor_eliminate(f in Formula::arbitrary_with(params())) {
            assert_equivalent_after_transform(f, &elimination::Xor);
        }

        #[test]
        fn equiv_eliminate(f in Formula::arbitrary_with(params())) {
            assert_equivalent_after_transform(f, &elimination::Equiv);
        }

        #[test]
        fn implication(f in Formula::arbitrary_with(params())) {
            assert_equivalent_after_transform(f, &elimination::Implication);
        }

        #[test]
        fn distrib_conjunction(f in Formula::arbitrary_with(params())) {
            assert_equivalent_after_transform(f, &distrib::DistributeConjunctionOverDisjunction);
        }

        #[test]
        fn distrib_disjunction(f in Formula::arbitrary_with(params())) {
            assert_equivalent_after_transform(f, &distrib::DistributeDisjunctionOverConjunction);
        }
    }
}

proptest! {
    // https://proptest-rs.github.io/proptest/proptest/tutorial/config.html
    #![proptest_config(ProptestConfig::with_cases(100))]
    #[test]
    fn applying_all_rules(f in Formula::arbitrary_with(params())) {
        // you can run this test alone with a single arbitrary formula like this:
        //
        // ```shell
        // $ PROPTEST_CASES=10 cargo test applying_all_rules --features=arbitrary -- --nocapture
        // EliminateConstants: ¬((¬c∧c→(⊤⊕c)∧⊥)↔¬(b∨⊥)) -> ¬(¬(¬c∧c)↔¬b)
        // NoDynamicConnective: ¬(¬(¬c∧c)↔¬b) -> ¬(¬(¬c∧c)↔¬b)
        // SortOperands: ¬(¬(¬c∧c)↔¬b) -> ¬(¬b↔¬(¬c∧c))
        // Negation: ¬(¬b↔¬(¬c∧c)) -> ¬(¬b↔¬⊥)
        // EliminateConstants: ¬(¬b↔¬⊥) -> ¬¬b
        // NoDynamicConnective: ¬¬b -> ¬¬b
        // DoubleNegation: ¬¬b -> b
        // ```
        let res = reduce_all_with_basic_rules(f.clone(), false, None);
        assert!(f.is_equivalent(&res));
        let eq_f = f.equivalent(res);
        assert!(
            eq_f.is_equivalent(&Formula::truth(true)),
        );

        let tr_f = reduce_all_with_basic_rules(eq_f, true, None);
        assert!(
            tr_f.is_equivalent(&Formula::truth(true)),
        );
    }
}
