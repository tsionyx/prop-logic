use std::ops::{BitAnd, BitOr, BitXor, Not};

use super::{
    super::{Equivalent, Implies},
    formula::{Directed, Formula},
};

impl<T> Not for Formula<T> {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self::Not(Box::new(self))
    }
}

impl<T> Not for Directed<T> {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Straight(x) => Self::Negated(x),
            Self::Negated(x) => Self::Straight(x),
        }
    }
}

impl<Rhs, T> BitAnd<Rhs> for Formula<T>
where
    Rhs: Into<Self>,
{
    type Output = Self;

    fn bitand(self, other: Rhs) -> Self::Output {
        Self::And(Box::new(self), Box::new(other.into()))
    }
}

impl<Rhs, T> BitOr<Rhs> for Formula<T>
where
    Rhs: Into<Self>,
{
    type Output = Self;

    fn bitor(self, other: Rhs) -> Self::Output {
        Self::Or(Box::new(self), Box::new(other.into()))
    }
}

impl<Rhs, T> BitXor<Rhs> for Formula<T>
where
    Rhs: Into<Self>,
{
    type Output = Self;

    fn bitxor(self, other: Rhs) -> Self::Output {
        Self::Xor(Box::new(self), Box::new(other.into()))
    }
}

impl<T, Rhs> Implies<Rhs> for Formula<T>
where
    Rhs: Into<Self>,
{
    fn implies(self, consequent: Rhs) -> Self {
        Self::Implies(Box::new(self), Box::new(consequent.into()))
    }
}

impl<T, Rhs> Equivalent<Rhs> for Formula<T>
where
    Rhs: Into<Self>,
{
    fn equivalent(self, other: Rhs) -> Self {
        Self::Equivalent(Box::new(self), Box::new(other.into()))
    }
}
