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
fn basic_rules<T>() -> Vec<Box<dyn RewritingRuleDebug<T>>>
where
    T: Ord + Clone,
{
    vec![
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
    ]
}

#[cfg(test)]
/// Apply the [basic rules][basic_rules]
/// in order to reduce the [`Formula`] to the max.
///
/// This function prints the debug information to be observed during testing.
fn reduce_all<T, I>(mut f: Formula<T>, allow_extend: bool, additional_rules: I) -> Formula<T>
where
    T: Debug + std::fmt::Display + Ord + Clone + 'static,
    Formula<T>: crate::TruthTabled<TT: std::fmt::Display>,
    I: IntoIterator<Item = Box<dyn RewritingRuleDebug<T>>>,
{
    use crate::TruthTabled as _;

    println!("{:=<80}", "");
    println!("{f:#}\n{}", f.get_truth_table());

    let all_rules: Vec<_> = basic_rules::<T>()
        .into_iter()
        .chain(additional_rules)
        .collect();
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

#[cfg(test)]
mod examples {
    use crate::truth_table::TruthTabled;

    use super::*;

    use Formula::*;

    #[test]
    fn many_conjunction() {
        let f = Equivalent(
            Box::new(Not(Box::new(Atomic('b')))),
            Box::new(And(
                Box::new(Atomic('b')),
                Box::new(Or(
                    Box::new(Not(Box::new(Atomic('b')))),
                    Box::new(Or(
                        Box::new(Not(Box::new(Atomic('c')))),
                        Box::new(Xor(Box::new(Atomic('a')), Box::new(Atomic('b')))),
                    )),
                )),
            )),
        );

        let plus_distrib: Box<dyn RewritingRuleDebug<char>> =
            Box::new(distrib::DistributeConjunctionOverDisjunction);
        let f2 = reduce_all(f, true, Some(plus_distrib));
        let expected = Formula::atom('a') & 'b' & 'c';
        assert!(f2.is_equivalent(&expected));
        assert_eq!(f2, expected);
    }

    #[test]
    fn negation_absorption() {
        let f = And(
            Box::new(Not(Box::new(Atomic('c')))),
            Box::new(And(
                Box::new(Atomic('b')),
                Box::new(Not(Box::new(Atomic('c')))),
            )),
        );

        let f2 = reduce_all(f, true, None);
        let expected = !Formula::from('c') & 'b';
        assert!(f2.is_equivalent(&expected));
    }

    #[test]
    fn repeating() {
        let f = (Atomic('p') & 'q') & !Atomic('q');
        let f2 = reduce_all(f, true, None);
        assert_eq!(f2, TruthValue(false));
    }

    #[test]
    fn sort_right_assoc() {
        let f = !Atomic('r') | (!Atomic('p') | !Atomic('q'));
        let f2 = reduce_all(f, true, None);
        assert_eq!(f2, !Atomic('p') | !Atomic('q') | !Atomic('r'));
    }

    #[test]
    fn group_same_operations() {
        // (p∧q)∧((¬p∧q)∧¬r)
        let f = (Atomic('p') & Atomic('q')) & ((!Atomic('p') & Atomic('q')) & !Atomic('r'));
        let f2 = reduce_all(f, true, None);
        assert_eq!(f2, TruthValue(false));
    }

    #[test]
    fn group_same_operations_2() {
        // ((p∧q)∧¬r)∧¬p
        let f = ((Atomic('p') & Atomic('q')) & !Atomic('r')) & !Atomic('p');
        let f2 = reduce_all(f.clone(), true, None);
        assert_eq!(f2, f); // TODO: find a way to reduce to `TruthValue(false)`
    }

    #[test]
    fn hard() {
        use crate::formula::Implies as _;

        // ¬((d⊕d)∨((b⊕((d→c)∨⊥))⊕d))
        let f = !((Formula::atom('d') ^ 'd')
            | ((Formula::atom('b') ^ ((Formula::from('d').implies('c')) | Formula::truth(false)))
                ^ 'd'));

        let f2 = reduce_all(f.clone(), true, None);
        assert!(f.is_equivalent(&f2));
    }
}
