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
/// Combination of [Disjunct]-s connected with
/// the [AND operation][crate::connective::Conjunction].
pub struct NormalForm<T> {
    repr: UnsortedVec<Disjunct<T>>,
}

impl<T> NormalForm<T> {
    /// Construct a new [`NormalForm`] from the series of [`Disjunct`]-s.
    pub fn new(disjuncts: impl IntoIterator<Item = Disjunct<T>>) -> Self {
        Self {
            repr: disjuncts.into_iter().collect(),
        }
    }
}

impl<T> From<NormalForm<T>> for Formula<T> {
    fn from(value: NormalForm<T>) -> Self {
        Series::<_, Conjunction>::new(value.repr.into_iter().map(Self::from)).compose()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Combination of literals connected with
/// the [OR operation][crate::connective::Disjunction].
pub struct Disjunct<T> {
    repr: UnsortedVec<Literal<T>>,
}

impl<T> Disjunct<T> {
    /// Construct a new [`Disjunct`] from the series of literals.
    pub fn new(lits: impl IntoIterator<Item = Literal<T>>) -> Self {
        Self {
            repr: lits.into_iter().collect(),
        }
    }
}

impl<T> From<Disjunct<T>> for Formula<T> {
    fn from(disjunct: Disjunct<T>) -> Self {
        Series::<_, Disjunction>::new(disjunct.repr.into_iter().map(Self::from)).compose()
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
