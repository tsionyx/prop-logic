//! Simplified representation of [`Formula`][super::super::Formula]
//! in the so-called _Negation normal form_.
//!
//! This representation consists of arbitrary set
//! of literals and sub-expressions connected only with
//! the [AND operation][crate::connective::Conjunction] and
//! the [OR operation][crate::connective::Disjunction]
//! without any additional constraints on the structure.
//!
//! This form can also be viewed as a special case of [`Formula`][super::super::Formula]
//! in the ([functionally complete][crate::connective::is_complete]) set of:
//! - [NOT][crate::connective::Negation];
//! - [AND][crate::connective::Conjunction];
//! - [OR][crate::connective::Disjunction].
//!
//! <https://en.wikipedia.org/wiki/Negation_normal_form>

use crate::{
    connective::{series, Conjunction, Disjunction},
    utils::vec::UnsortedVec,
};

use super::super::{Formula, Signed};

type Literal<T> = Signed<T>;

#[derive(Debug, PartialEq, Eq, Clone)]
/// Arbitrary set of literals and sub-expressions connected with
/// the [AND operation] or [OR operation].
pub enum NormalForm<T> {
    /// The simplest type of [`NormalForm`] with no deeper propositional structure.
    Literal(Literal<T>),

    /// A number of conjunctions of the sub-formulae.
    ///
    /// The empty set represents
    /// a [`truth`][crate::connective::Evaluable::tautology] value.
    And(UnsortedVec<Self>),

    /// A number of disunctions of the sub-formulae.
    ///
    /// The empty set represents
    /// a [`false`][crate::connective::Evaluable::contradiction] value.
    Or(UnsortedVec<Self>),
}

impl<T> From<NormalForm<T>> for Formula<T> {
    fn from(value: NormalForm<T>) -> Self {
        match value {
            NormalForm::Literal(lit) => lit.into(),
            NormalForm::And(subs) => series(&Conjunction, subs.into_iter().map(Self::from)),
            NormalForm::Or(subs) => series(&Disjunction, subs.into_iter().map(Self::from)),
        }
    }
}

impl<T> From<Formula<T>> for NormalForm<T> {
    fn from(_formula: Formula<T>) -> Self {
        todo!("use the rewriting rules: https://en.wikipedia.org/wiki/Logical_equivalence")
    }
}
