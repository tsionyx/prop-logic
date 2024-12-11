//! Optimized representation of [`Formula`][super::super::Formula]
//! in the so-called _Algebraic normal form_
//! aka _Zhegalkin normal form_.
//!
//! This representation consists of one or more [terms][Term] connected
//! with the [XOR operation][crate::connective::ExclusiveDisjunction]
//! where every term can be only:
//! - a constant [Truth][crate::connective::Truth];
//! - or a series of [Conjunction][crate::connective::Conjunction]
//!   of variables in **non-negated** form.
//!
//! This form can also be viewed as a special case of [`Formula`][super::super::Formula]
//! in the ([functionally complete][crate::connective::is_basis]) basis of:
//! - [1][crate::connective::Truth];
//! - [AND][crate::connective::Conjunction];
//! - [XOR][crate::connective::ExclusiveDisjunction].
//!
//! <https://en.wikipedia.org/wiki/Algebraic_normal_form>
//! <https://en.wikipedia.org/wiki/Zhegalkin_polynomial>

use crate::{
    connective::{series, Conjunction, Evaluable as _, ExclusiveDisjunction},
    utils::vec::UnsortedVec,
};

use super::super::Formula;

#[derive(Debug, PartialEq, Eq, Clone)]
/// Combination of [Term]-s connected with
/// the [XOR operation][crate::connective::ExclusiveDisjunction].
pub struct NormalForm<T> {
    repr: UnsortedVec<Term<T>>,
}

impl<T> NormalForm<T> {
    /// Construct a new [`NormalForm`] from the series of [`Term`]-s.
    pub fn new(terms: impl IntoIterator<Item = Term<T>>) -> Self {
        Self {
            repr: terms.into_iter().collect(),
        }
    }
}

impl<T> From<NormalForm<T>> for Formula<T> {
    fn from(value: NormalForm<T>) -> Self {
        series(
            &ExclusiveDisjunction,
            value.repr.into_iter().map(Self::from),
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Combination of variables in **non-negated** form connected using
/// the [AND operation][crate::connective::Conjunction]
/// or just the constant [truth value][crate::connective::Truth].
pub enum Term<T> {
    /// Represent the logical constant [tautology][crate::connective::Truth].
    Truth,

    /// A combination of variables connected using
    /// the [AND operation][crate::connective::Conjunction].
    Conjunct(UnsortedVec<T>),
}

impl<T> From<Term<T>> for Formula<T> {
    fn from(term: Term<T>) -> Self {
        match term {
            Term::Truth => Self::tautology(),
            Term::Conjunct(vars) => series(&Conjunction, vars.into_iter().map(Self::from)),
        }
    }
}

impl<T> From<Formula<T>> for NormalForm<T> {
    fn from(_formula: Formula<T>) -> Self {
        todo!("use the rewriting rules: https://en.wikipedia.org/wiki/Logical_equivalence")
    }
}
