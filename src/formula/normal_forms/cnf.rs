//! Optimized representation of [`Formula`][super::super::Formula]
//! in the so-called _Conjunctive normal form_.
//!
//! This representation consists of one or more terms called [Disjunct], connected
//! with the [AND operation][crate::connective::Conjunction]
//! where every disjunct is the combination of one or more literals
//! connected with the [OR operation][crate::connective::Disjunction].
//!
//! This form can also be viewed as a special case of [`Formula`][super::super::Formula]
//! in the ([functionally complete][crate::connective::is_complete]) set of:
//! - [NOT][crate::connective::Negation];
//! - [AND][crate::connective::Conjunction];
//! - [OR][crate::connective::Disjunction].
//!
//! <https://en.wikipedia.org/wiki/Conjunctive_normal_form>
use super::{
    super::{equivalences::RewritingRuleDebug, Formula, Signed as Literal},
    error::Error,
    NormalForm as NormalFormTrait,
};

pub type SumTerm<T> = super::Disjunct<Literal<T>>;

impl<T> From<SumTerm<T>> for Formula<T> {
    fn from(value: SumTerm<T>) -> Self {
        value.compose()
    }
}

/// Combination of [Disjunct][super::Disjunct]-s connected with
/// the [AND operation][crate::connective::Conjunction].
pub type NormalForm<T> = super::Conjunct<SumTerm<T>>;

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
        // To promote it to CNF, we need to apply one more rule
        // to force the disjunction to happen only between literals.
        let distrib_rule: Box<dyn RewritingRuleDebug<V>> =
            Box::new(super::super::equivalences::distrib::DistributeDisjunctionOverConjunction);

        rules.into_iter().chain(Some(distrib_rule)).collect()
    }
}
