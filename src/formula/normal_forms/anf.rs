//! Optimized representaion of [`Formula`][super::super::Formula]
//! in the so-called _Algebraic normal form_
//! aka _Zhegalkin normal form_.
//!
//! This representaion consist of one or more [terms][Term] connected
//! with the [XOR operation][crate::connective::ExclusiveDisjunction]
//! where every term can be only:
//! - a constant [Truth][crate::connective::Truth];
//! - or a series of [Conjunction][crate::connective::Conjunction]
//!   of atoms in **non-negated** form.
//!
//! This form can also be viewed as a special case of [`Formula`][super::super::Formula]
//! in the ([functionally complete][crate::connective::is_basis]) basis of:
//! - [1][crate::connective::Truth];
//! - [AND][crate::connective::Conjunction];
//! - [XOR][crate::connective::ExclusiveDisjunction].
//!
//! <https://en.wikipedia.org/wiki/Algebraic_normal_form>
//! <https://en.wikipedia.org/wiki/Zhegalkin_polynomial>

use crate::connective::Evaluable as _;

use super::super::{
    ops::{And as _, Xor as _},
    Formula, Variable,
};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Combination of [Term]-s connected with
/// the [XOR operation][crate::connective::ExclusiveDisjunction].
pub struct NormalForm<T> {
    repr: Vec<Term<T>>,
}

impl<T> NormalForm<T> {
    /// Construct a new [`NormalForm`] from the series of [`Term`]-s.
    pub fn new(terms: impl IntoIterator<Item = Term<T>>) -> Self {
        Self {
            repr: terms.into_iter().collect(),
        }
    }
}

impl<T> From<NormalForm<T>> for Formula<Variable<T>> {
    fn from(value: NormalForm<T>) -> Self {
        value
            .repr
            .into_iter()
            .rfold(Self::contradiction(), |acc, term| term.xor(acc))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Combination of [Variable]-s connected with
/// the [AND operation][crate::connective::Conjunction]
/// or just the constant [truth value][crate::connective::Truth].
pub enum Term<T> {
    /// Represent the logical constant [tautology][crate::connective::Truth].
    Truth,

    /// Combination of [Variable]-s connected with
    /// the [AND operation][crate::connective::Conjunction].
    Conjunct(Vec<Variable<T>>),
}

impl<T> From<Term<T>> for Formula<Variable<T>> {
    fn from(term: Term<T>) -> Self {
        match term {
            Term::Truth => Self::tautology(),
            Term::Conjunct(vars) => vars
                .into_iter()
                .rfold(Self::tautology(), |acc, var| var.and(acc)),
        }
    }
}

impl<T> From<Formula<Variable<T>>> for NormalForm<T> {
    fn from(_formula: Formula<Variable<T>>) -> Self {
        todo!("use the rewriting rules: https://en.wikipedia.org/wiki/Logical_equivalence")
    }
}
