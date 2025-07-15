//! The rules to convert a [`Formula`][super::formula::Formula]
//! to the equivalent one by using common equivalence laws.
//!
//! <https://en.wikipedia.org/wiki/Logical_equivalence>

pub(crate) mod absorption;
pub(crate) mod canonical;
pub(crate) mod constant;
pub(crate) mod distrib;
pub(crate) mod elimination;
pub(crate) mod eq;
pub(crate) mod neg;
#[cfg(all(test, feature = "arbitrary"))]
mod prop_test;
pub(crate) mod sort;
mod traits;

use std::fmt::Debug;

use crate::utils::upcast::{Upcast, UpcastFrom};

use super::formula::Formula;

pub use traits::RewritingRule;

/// Apply many rules to reduce the [`Formula`] to its max.
///
/// The rules will be applied in a loop until no more reduction can be reached.
pub fn apply_many<T>(
    mut f: Formula<T>,
    rules: &[&dyn RewritingRule<T>],
    allow_extend: bool,
) -> Formula<T>
where
    // requires 'static for Formula<T>: PartialEq
    T: PartialEq + Clone + 'static,
{
    loop {
        let original = f.clone();
        for rule in rules {
            f = rule.apply_all(f, allow_extend);
        }

        // irreducible in terms of all the rules
        if f == original {
            return f;
        }
    }
}

/// The [`RewritingRule`] able to be [`Debug`]-ged.
pub trait RewritingRuleDebug<T>: RewritingRule<T> + Debug + Upcast<dyn RewritingRule<T>> {}

impl<T, R> RewritingRuleDebug<T> for R where
    R: RewritingRule<T> + Debug + Upcast<dyn RewritingRule<T>>
{
}

impl<'a, T, R: RewritingRule<T> + 'a> UpcastFrom<R> for dyn RewritingRule<T> + 'a {
    fn up_from(value: &R) -> &Self {
        value
    }

    fn up_from_mut(value: &mut R) -> &mut Self {
        value
    }
}

#[cfg(test)]
mod examples {
    use crate::truth_table::TruthTabled;

    use super::*;

    /// Apply the [basic rules][basic_rules]
    /// in order to reduce the [`Formula`] to the max.
    ///
    /// This function prints the debug information to be observed during testing.
    pub fn reduce_all_with_basic_rules<T, I>(
        f: Formula<T>,
        allow_extend: bool,
        additional_rules: I,
    ) -> Formula<T>
    where
        T: Debug + std::fmt::Display + Ord + Clone + 'static,
        Formula<T>: TruthTabled<TT: std::fmt::Display>,
        I: IntoIterator<Item = Box<dyn RewritingRuleDebug<T>>>,
    {
        let basic_rules: Vec<Box<dyn RewritingRuleDebug<T>>> = vec![
            Box::new(constant::EliminateConstants),
            Box::new(canonical::NoDynamicConnective),
            Box::new(neg::DoubleNegation),
            Box::new(sort::SortAssociativeOperators),
            Box::new(eq::Idempotence),
            Box::new(eq::Negation),
            Box::new(absorption::Absorption),
            Box::new(absorption::AbsorptionWithNeg),
            Box::new(elimination::DeMorgan),
            Box::new(elimination::XorEquivNegation),
            Box::new(elimination::Implication),
            Box::new(elimination::Xor),
            Box::new(elimination::Equiv),
        ];

        reduce_all(
            f,
            allow_extend,
            basic_rules.into_iter().chain(additional_rules),
        )
    }

    /// Apply the set of rules
    /// in order to reduce the [`Formula`] to the max.
    ///
    /// This function prints the debug information to be observed during testing.
    pub fn reduce_all<T, I>(mut f: Formula<T>, allow_extend: bool, rules: I) -> Formula<T>
    where
        T: Debug + std::fmt::Display + Ord + Clone + 'static,
        Formula<T>: TruthTabled<TT: std::fmt::Display>,
        I: IntoIterator<Item = Box<dyn RewritingRuleDebug<T>>>,
    {
        println!("{:=<80}", "");
        println!("{f:#}\n{}", f.get_truth_table());

        let all_rules: Vec<_> = rules.into_iter().collect();
        loop {
            let original = f.clone();
            for rule in &all_rules {
                let f2 = rule.apply_all(f.clone(), allow_extend);
                if f != f2 {
                    println!("{rule:?}: \n{f:#} \n{f2:#}");
                    // println!("{}", f2.get_truth_table());
                    f = f2;
                }
            }

            if f == original {
                println!("{f:#}: ({f:#?})");
                println!("{}", f.get_truth_table());
                return f;
            }
        }
    }

    #[test]
    fn many_conjunction() {
        use crate::formula::Equivalent as _;

        let f = (!Formula::atom('b')).equivalent(
            Formula::atom('b')
                & (!Formula::atom('b') | (!Formula::atom('c') | (Formula::atom('a') ^ 'b'))),
        );

        let plus_distrib: Box<dyn RewritingRuleDebug<char>> =
            Box::new(distrib::DistributeConjunctionOverDisjunction);
        let f2 = reduce_all_with_basic_rules(f, true, Some(plus_distrib));
        let expected = Formula::atom('a') & 'b' & 'c';
        assert!(f2.is_equivalent(&expected));
        assert_eq!(f2, expected);
    }

    #[test]
    fn negation_absorption() {
        let f = (!Formula::atom('c')) & (Formula::atom('b') & !Formula::atom('c'));

        let f2 = reduce_all_with_basic_rules(f, true, None);
        let expected = !Formula::atom('c') & 'b';
        assert!(f2.is_equivalent(&expected));
    }

    #[test]
    fn repeating() {
        let f = (Formula::atom('p') & 'q') & !Formula::atom('q');
        let f2 = reduce_all_with_basic_rules(f, true, None);
        assert_eq!(f2, Formula::TruthValue(false));
    }

    #[test]
    fn sort_right_assoc() {
        let f = !Formula::atom('r') | (!Formula::atom('p') | !Formula::atom('q'));
        let f2 = reduce_all_with_basic_rules(f, true, None);
        assert_eq!(
            f2,
            !Formula::atom('p') | !Formula::atom('q') | !Formula::atom('r')
        );
    }

    #[test]
    fn group_same_operations() {
        // (p∧q)∧((¬p∧q)∧¬r)
        let f = (Formula::atom('p') & 'q') & ((!Formula::atom('p') & 'q') & !Formula::atom('r'));
        let f2 = reduce_all_with_basic_rules(f, true, None);
        assert_eq!(f2, Formula::TruthValue(false));
    }

    #[test]
    fn group_same_operations_2() {
        // ((p∧q)∧¬r)∧¬p
        let f = ((Formula::atom('p') & 'q') & !Formula::atom('r')) & !Formula::atom('p');
        let f2 = reduce_all_with_basic_rules(f.clone(), true, None);
        assert_eq!(f2, f); // TODO: find a way to reduce to `TruthValue(false)`
    }

    #[test]
    fn hard() {
        use crate::formula::Implies as _;

        // ¬((d⊕d)∨((b⊕((d→c)∨⊥))⊕d))
        let f = !((Formula::atom('d') ^ 'd')
            | ((Formula::atom('b') ^ ((Formula::from('d').implies('c')) | Formula::truth(false)))
                ^ 'd'));

        let f2 = reduce_all_with_basic_rules(f.clone(), true, None);
        assert!(f.is_equivalent(&f2));
    }

    #[test]
    fn conversion_to_literals_with_associative() {
        let f = Formula::atom('a') & (Formula::atom('a') | 'b') & (!(!Formula::atom('b')));

        let f2 = sort::SortAssociativeOperators
            .reduce(f.clone())
            .unwrap_err();
        assert_eq!(f, f2);
    }
}
