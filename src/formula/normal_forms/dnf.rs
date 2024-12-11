//! Optimized representaion of [`Formula`][super::super::Formula]
//! in the so-called _Disjunctive normal form_.
//!
//! This representaion consist of one or more terms called [Conjunct], connected
//! with the [OR operation][crate::connective::Disjunction]
//! where every conjunct is the combination of one or more [Literal]-s
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
    super::{
        ops::{And as _, Or as _},
        Formula, Variable,
    },
    Literal,
};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Combination of [Disjunct]-s connected with
/// the [OR operation][crate::connective::Disjunction].
pub struct NormalForm<T> {
    repr: Vec<Conjunct<T>>,
}

impl<T> NormalForm<T> {
    /// Construct a new [`NormalForm`] from the series of [`Conjunct`]-s.
    pub fn new(conjuncts: impl IntoIterator<Item = Conjunct<T>>) -> Self {
        Self {
            repr: conjuncts.into_iter().collect(),
        }
    }
}

impl<T> From<NormalForm<T>> for Formula<Variable<T>> {
    fn from(value: NormalForm<T>) -> Self {
        value
            .repr
            .into_iter()
            .rfold(Self::contradiction(), |acc, conjunct| conjunct.or(acc))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Combination of [Literal]-s connected with
/// the [AND operation][crate::connective::Conjunction].
pub struct Conjunct<T> {
    repr: Vec<Literal<T>>,
}

impl<T> Conjunct<T> {
    /// Construct a new [`Conjunct`] from the series of [`Literal`]-s.
    pub fn new(lits: impl IntoIterator<Item = Literal<T>>) -> Self {
        Self {
            repr: lits.into_iter().collect(),
        }
    }
}

impl<T> From<Conjunct<T>> for Formula<Variable<T>> {
    fn from(conjunct: Conjunct<T>) -> Self {
        conjunct
            .repr
            .into_iter()
            .rfold(Self::tautology(), |acc, lit| lit.and(acc))
    }
}
