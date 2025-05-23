//! Simplified representaion of [`Formula`][super::super::Formula]
//! in the so-called _Negation normal form_.
//!
//! This representaion consist of arbitrary set
//! of [Literal]-s and sub-expressions connected only with
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

use crate::connective::Evaluable as _;

use super::super::{
    ops::{And as _, Or as _},
    Formula, Literal, Variable,
};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Arbitrary set of [Literal]-s and sub-expressions connected with
/// the [AND operation] or [OR operation].
pub enum NormalForm<T> {
    /// The simplest type of [`NormalForm`] with no deeper propositional structure.
    Literal(Literal<T>),

    /// A number of conjunctions of the sub-formulae.
    ///
    /// The empty set represents
    /// a [`truth`][crate::connective::Evaluable::tautology] value.
    And(Vec<Self>),

    /// A number of disunctions of the sub-formulae.
    ///
    /// The empty set represents
    /// a [`false`][crate::connective::Evaluable::contradiction] value.
    Or(Vec<Self>),
}

impl<T> From<NormalForm<T>> for Formula<Variable<T>> {
    fn from(value: NormalForm<T>) -> Self {
        match value {
            NormalForm::Literal(lit) => lit.into(),
            NormalForm::And(subs) => subs
                .into_iter()
                .rfold(Self::tautology(), |acc, sub| Self::from(sub).and(acc)),
            NormalForm::Or(subs) => subs
                .into_iter()
                .rfold(Self::contradiction(), |acc, sub| Self::from(sub).or(acc)),
        }
    }
}

impl<T> From<Formula<Variable<T>>> for NormalForm<T> {
    fn from(_formula: Formula<Variable<T>>) -> Self {
        todo!("use the rewriting rules: https://en.wikipedia.org/wiki/Logical_equivalence")
    }
}
