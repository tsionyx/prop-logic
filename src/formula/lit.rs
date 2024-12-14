use std::ops::Not;

use crate::utils::zst::Void;

pub use super::{Formula, Variable};

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
/// A [literal][Literal] is a kind of generalization of a [Variable]
/// that can be either:
/// - in plain form, or just
///   the variable's [identity][crate::connective::LogicalIdentity];
/// - in negated form, or
///   the variable's [negation][crate::connective::Negation];
pub enum Literal<T> {
    /// The [Variable] itself.
    Var(Variable<T>),

    /// The [negated][crate::connective::Negation] [Variable].
    Neg(Variable<T>),
}

/// The most primitive instantiation of the [`Literal`]
/// without any additional info attached
/// optimized for memory usage.
pub type Lit = Literal<Void>;

impl<T> From<Variable<T>> for Literal<T> {
    fn from(value: Variable<T>) -> Self {
        Self::Var(value)
    }
}

impl<T> Not for Literal<T> {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Var(var) => Self::Neg(var),
            Self::Neg(var) => Self::Var(var),
        }
    }
}

impl<T> Not for Variable<T> {
    type Output = Literal<T>;

    fn not(self) -> Self::Output {
        Literal::Neg(self)
    }
}

impl<T> From<Literal<T>> for Formula<Variable<T>> {
    fn from(lit: Literal<T>) -> Self {
        match lit {
            Literal::Var(var) => Self::atomic(var),
            Literal::Neg(var) => !Self::atomic(var),
        }
    }
}
