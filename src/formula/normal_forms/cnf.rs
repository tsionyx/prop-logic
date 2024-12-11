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

use crate::{
    connective::{series, Conjunction, Disjunction},
    utils::vec::UnsortedVec,
};

use super::super::{Formula, Signed};

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
        series(&Conjunction, value.repr.into_iter().map(Self::from))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Combination of literals connected with
/// the [OR operation][crate::connective::Disjunction].
pub struct Disjunct<T> {
    repr: UnsortedVec<Signed<T>>,
}

impl<T> Disjunct<T> {
    /// Construct a new [`Disjunct`] from the series of literals.
    pub fn new(lits: impl IntoIterator<Item = Signed<T>>) -> Self {
        Self {
            repr: lits.into_iter().collect(),
        }
    }
}

impl<T> From<Disjunct<T>> for Formula<T> {
    fn from(disjunct: Disjunct<T>) -> Self {
        series(&Disjunction, disjunct.repr.into_iter().map(Self::from))
    }
}

impl<T> From<Formula<T>> for NormalForm<T> {
    fn from(_formula: Formula<T>) -> Self {
        todo!("use the rewriting rules: https://en.wikipedia.org/wiki/Logical_equivalence")
    }
}
