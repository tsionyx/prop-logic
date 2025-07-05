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
use std::fmt::Debug;

use crate::{
    connective::{Conjunction, Disjunction, Series},
    utils::vec::UnsortedVec,
};

use super::{
    super::{equivalences::RewritingRuleDebug, Formula, Signed as Literal},
    error::Error,
    NormalForm as NormalFormTrait,
};

#[derive(Debug, PartialEq, Eq, Clone)]
/// Combination of [Conjunct]-s connected with
/// the [OR operation][crate::connective::Disjunction].
pub struct NormalForm<T> {
    repr: UnsortedVec<Conjunct<T>>,
}

impl<T> NormalForm<T> {
    /// Construct a new [`NormalForm`] from the series of [`Conjunct`]-s.
    pub fn new(conjuncts: impl IntoIterator<Item = Conjunct<T>>) -> Self {
        Self {
            repr: conjuncts.into_iter().collect(),
        }
    }
}

impl<T> From<NormalForm<T>> for Formula<T> {
    fn from(value: NormalForm<T>) -> Self {
        Series::<_, Disjunction>::new(value.repr.into_iter().map(Self::from)).compose()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Combination of literals connected with
/// the [AND operation][crate::connective::Conjunction].
pub struct Conjunct<T> {
    repr: UnsortedVec<Literal<T>>,
}

impl<T> Conjunct<T> {
    /// Construct a new [`Conjunct`] from the series of literals.
    pub fn new(lits: impl IntoIterator<Item = Literal<T>>) -> Self {
        Self {
            repr: lits.into_iter().collect(),
        }
    }
}

impl<T> From<Conjunct<T>> for Formula<T> {
    fn from(conjunct: Conjunct<T>) -> Self {
        Series::<_, Conjunction>::new(conjunct.repr.into_iter().map(Self::from)).compose()
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
