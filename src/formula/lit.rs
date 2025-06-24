use std::ops::Not;

use crate::utils::zst::Void;

pub use super::Variable;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
/// The wrapper around a value to represent the attachment of a sign to it.
pub enum Signed<T> {
    /// The value itself.
    Pos(T),
    /// The negated value.
    Neg(T),
}

impl<T> From<T> for Signed<T> {
    fn from(value: T) -> Self {
        Self::Pos(value)
    }
}

impl<T> AsRef<T> for Signed<T> {
    fn as_ref(&self) -> &T {
        match self {
            Self::Pos(p) | Self::Neg(p) => p,
        }
    }
}

impl<T> Not for Signed<T> {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Pos(var) => Self::Neg(var),
            Self::Neg(var) => Self::Pos(var),
        }
    }
}

/// A [literal][Literal] is a kind of generalization of a [Variable]
/// that can be either:
/// - in plain form, or just
///   the variable's [identity][crate::connective::LogicalIdentity];
/// - in negated form, or
///   the variable's [negation][crate::connective::Negation];
pub type Literal<T> = Signed<Variable<T>>;

/// The most primitive instantiation of the [`Literal`]
/// without any additional info attached
/// optimized for memory usage.
pub type Lit = Literal<Void>;

impl<T> Not for Variable<T> {
    type Output = Literal<T>;

    fn not(self) -> Self::Output {
        Literal::Neg(self)
    }
}
