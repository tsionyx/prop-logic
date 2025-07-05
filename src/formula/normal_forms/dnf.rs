//! Optimized representation of [`Formula`][super::super::Formula]
//! in the so-called _Disjunctive normal form_.
//!
//! This representation consists of one or more terms called [Conjunct], connected
//! with the [OR operation][crate::connective::Disjunction]
//! where every conjunct is the combination of one or more literals
//! connected with the [AND operation][crate::connective::Conjunction].
//!
//! This form can also be viewed as a special case of [`Formula`][super::super::Formula]
//! in the ([functionally complete][crate::connective::is_complete]) set of:
//! - [NOT][crate::connective::Negation];
//! - [AND][crate::connective::Conjunction];
//! - [OR][crate::connective::Disjunction].
//!
//! <https://en.wikipedia.org/wiki/Disjunctive_normal_form>
use super::{
    super::{equivalences::RewritingRuleDebug, Formula, Signed as Literal},
    error::Error,
    NormalForm as NormalFormTrait,
};

/// <https://en.wikipedia.org/wiki/Product_term>
pub type ProductTerm<T> = super::Conjunct<Literal<T>>;

impl<T> From<ProductTerm<T>> for Formula<T> {
    fn from(value: ProductTerm<T>) -> Self {
        value.compose()
    }
}

/// Combination of [Conjunct][super::Conjunct]-s connected with
/// the [OR operation][crate::connective::Disjunction].
pub type NormalForm<T> = super::Disjunct<ProductTerm<T>>;

impl<T> From<NormalForm<T>> for Formula<T> {
    fn from(value: NormalForm<T>) -> Self {
        value.compose()
    }
}

impl<T> TryFrom<Formula<T>> for NormalForm<T>
where
    T: Clone + PartialEq + 'static,
{
    type Error = Error;

    fn try_from(formula: Formula<T>) -> Result<Self, Self::Error> {
        let _formula = Self::prepare(formula);
        todo!()
    }
}

type DummyType = u8;

impl<T> NormalFormTrait<T> for NormalForm<T>
where
    T: PartialEq + Clone + 'static,
{
    fn rules<V: PartialEq + Clone>() -> Vec<Box<dyn RewritingRuleDebug<V>>> {
        let rules = super::NegationNormalForm::<DummyType>::rules();

        // At this point, the formula should be in NNF form already.
        // To promote it to DNF, we need to apply one more rule
        // to force the conjunction to happen only between literals.
        let distrib_rule: Box<dyn RewritingRuleDebug<V>> =
            Box::new(super::super::equivalences::distrib::DistributeConjunctionOverDisjunction);

        rules.into_iter().chain(Some(distrib_rule)).collect()
    }
}
