use std::ops::{BitAnd, BitOr, BitXor, Not};

use super::{
    super::{Equivalent, Implies},
    formula::{Directed, Formula},
};

impl<T> Not for Box<Formula<T>> {
    type Output = Formula<T>;

    fn not(self) -> Self::Output {
        Formula::Not(self)
    }
}

impl<T> Not for Formula<T> {
    type Output = Self;

    fn not(self) -> Self::Output {
        !Box::new(self)
    }
}

impl<Rhs, T> BitAnd<Rhs> for Box<Formula<T>>
where
    Rhs: Into<Self>,
{
    type Output = Formula<T>;

    fn bitand(self, other: Rhs) -> Self::Output {
        Formula::And(self, other.into())
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
        Box::new(self) & other.into()
    }
}

impl<T> BitAnd<Box<Self>> for Formula<T> {
    type Output = Self;

    fn bitand(self, other: Box<Self>) -> Self::Output {
        Box::new(self) & other
    }
}

impl<Rhs, T> BitOr<Rhs> for Box<Formula<T>>
where
    Rhs: Into<Self>,
{
    type Output = Formula<T>;

    fn bitor(self, other: Rhs) -> Self::Output {
        Formula::Or(self, other.into())
    }
}

impl<Rhs, T> BitOr<Rhs> for Formula<T>
where
    Rhs: Into<Self>,
{
    type Output = Self;

    fn bitor(self, other: Rhs) -> Self::Output {
        Box::new(self) | other.into()
    }
}

impl<T> BitOr<Box<Self>> for Formula<T> {
    type Output = Self;

    fn bitor(self, other: Box<Self>) -> Self::Output {
        Box::new(self) | other
    }
}

impl<Rhs, T> BitXor<Rhs> for Box<Formula<T>>
where
    Rhs: Into<Self>,
{
    type Output = Formula<T>;

    fn bitxor(self, other: Rhs) -> Self::Output {
        Formula::Xor(self, other.into())
    }
}

impl<Rhs, T> BitXor<Rhs> for Formula<T>
where
    Rhs: Into<Self>,
{
    type Output = Self;

    fn bitxor(self, other: Rhs) -> Self::Output {
        Box::new(self) ^ other.into()
    }
}

impl<T> BitXor<Box<Self>> for Formula<T> {
    type Output = Self;

    fn bitxor(self, other: Box<Self>) -> Self::Output {
        Box::new(self) ^ other
    }
}

impl<T, Rhs> Implies<Rhs> for Formula<T>
where
    Rhs: Into<Self>,
{
    fn implies(self, consequent: Rhs) -> Self {
        self.implies(Box::new(consequent.into()))
    }
}

impl<T> Implies<Box<Self>> for Formula<T> {
    fn implies(self, consequent: Box<Self>) -> Self {
        Self::Implies(Box::new(self), consequent)
    }
}

impl<T, Rhs> Equivalent<Rhs> for Formula<T>
where
    Rhs: Into<Self>,
{
    fn equivalent(self, other: Rhs) -> Self {
        self.equivalent(Box::new(other.into()))
    }
}

impl<T> Equivalent<Box<Self>> for Formula<T> {
    fn equivalent(self, other: Box<Self>) -> Self {
        Self::Equivalent(Box::new(self), other)
    }
}
