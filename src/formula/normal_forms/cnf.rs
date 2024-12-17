//! Optimized representaion of [`Formula`][super::super::Formula]
//! in the so-called _Conjunctive normal form_.
//!
//! This representaion consist of one or more terms called [Disjunct], connected
//! with the [AND operation][crate::connective::Conjunction]
//! where every disjunct is the combination of one or more [Literal]-s
//! connected with the [OR operation][crate::connective::Disjunction].
//!
//! This form can also be viewed as a special case of [`Formula`][super::super::Formula]
//! in the ([functionally complete][crate::connective::is_complete]) set of:
//! - [NOT][crate::connective::Negation];
//! - [AND][crate::connective::Conjunction];
//! - [OR][crate::connective::Disjunction].
//!
//! <https://en.wikipedia.org/wiki/Conjunctive_normal_form>

use crate::connective::Evaluable as _;

use super::super::{
    ops::{And as _, Or as _},
    Formula, Literal, Variable,
};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Combination of [Disjunct]-s connected with
/// the [AND operation][crate::connective::Conjunction].
pub struct NormalForm<T> {
    repr: Vec<Disjunct<T>>,
}

impl<T> NormalForm<T> {
    /// Construct a new [`NormalForm`] from the series of [`Disjunct`]-s.
    pub fn new(disjuncts: impl IntoIterator<Item = Disjunct<T>>) -> Self {
        Self {
            repr: disjuncts.into_iter().collect(),
        }
    }
}

impl<T> From<NormalForm<T>> for Formula<Variable<T>> {
    fn from(value: NormalForm<T>) -> Self {
        value
            .repr
            .into_iter()
            .rfold(Self::tautology(), |acc, disjunct| disjunct.and(acc))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Combination of [Literal]-s connected with
/// the [OR operation][crate::connective::Disjunction].
pub struct Disjunct<T> {
    repr: Vec<Literal<T>>,
}

impl<T> Disjunct<T> {
    /// Construct a new [`Disjunct`] from the series of [`Literal`]-s.
    pub fn new(lits: impl IntoIterator<Item = Literal<T>>) -> Self {
        Self {
            repr: lits.into_iter().collect(),
        }
    }
}

impl<T> From<Disjunct<T>> for Formula<Variable<T>> {
    fn from(disjunct: Disjunct<T>) -> Self {
        disjunct
            .repr
            .into_iter()
            .rfold(Self::contradiction(), |acc, lit| lit.or(acc))
    }
}

impl<T> From<Formula<Variable<T>>> for NormalForm<T> {
    fn from(_formula: Formula<Variable<T>>) -> Self {
        todo!("use the rewriting rules: https://en.wikipedia.org/wiki/Logical_equivalence")
    }
}
