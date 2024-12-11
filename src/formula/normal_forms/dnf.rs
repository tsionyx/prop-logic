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

use crate::{
    connective::{series, Conjunction, Disjunction},
    utils::vec::UnsortedVec,
};

use super::super::{Formula, Signed};

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
        series(&Disjunction, value.repr.into_iter().map(Self::from))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Combination of literals connected with
/// the [AND operation][crate::connective::Conjunction].
pub struct Conjunct<T> {
    repr: UnsortedVec<Signed<T>>,
}

impl<T> Conjunct<T> {
    /// Construct a new [`Conjunct`] from the series of literals.
    pub fn new(lits: impl IntoIterator<Item = Signed<T>>) -> Self {
        Self {
            repr: lits.into_iter().collect(),
        }
    }
}

impl<T> From<Conjunct<T>> for Formula<T> {
    fn from(conjunct: Conjunct<T>) -> Self {
        series(&Conjunction, conjunct.repr.into_iter().map(Self::from))
    }
}

impl<T> From<Formula<T>> for NormalForm<T> {
    fn from(_formula: Formula<T>) -> Self {
        todo!("use the rewriting rules: https://en.wikipedia.org/wiki/Logical_equivalence")
    }
}
