use std::ops::{BitAnd, BitOr, BitXor, Not};

use super::{
    super::{Equivalent, Implies},
    formula::Formula,
};

impl<T> Not for Formula<T> {
    type Output = Self;

    fn not(self) -> Self::Output {
        // Do not use the `functions::Negation.try_reduce([f])`
        // since it recursively calls the same function again.
        if let Self::TruthValue(value) = &self {
            Self::truth(!*value)
        } else {
            Self::not(self)
        }
    }
}

impl<Rhs, T> BitAnd<Rhs> for Formula<T>
where
    Rhs: Into<Self>,
{
    type Output = Self;

    fn bitand(self, e: Rhs) -> Self::Output {
        Self::and(self, e.into())
    }
}

impl<Rhs, T> BitOr<Rhs> for Formula<T>
where
    Rhs: Into<Self>,
{
    type Output = Self;

    fn bitor(self, e: Rhs) -> Self::Output {
        Self::or(self, e.into())
    }
}

impl<Rhs, T> BitXor<Rhs> for Formula<T>
where
    Rhs: Into<Self>,
{
    type Output = Self;

    fn bitxor(self, e: Rhs) -> Self::Output {
        Self::xor(self, e.into())
    }
}

// special implementations instead of the blanket ones,
// since there is no special operators like `->` and `<->`

impl<T, Rhs> Implies<Rhs> for Formula<T>
where
    Rhs: Into<Self>,
{
    fn implies(self, rhs: Rhs) -> Self {
        use crate::connective::TruthFn as _;

        let f1 = self;
        let f2 = rhs.into();

        crate::connective::MaterialImplication
            .fold([f1, f2])
            .unwrap_or_else(|[f1, f2]| Self::implies(f1, f2))
    }
}

impl<T, Rhs> Equivalent<Rhs> for Formula<T>
where
    Rhs: Into<Self>,
{
    fn equivalent(self, rhs: Rhs) -> Self {
        use crate::connective::TruthFn as _;

        let f1 = self;
        let f2 = rhs.into();

        crate::connective::LogicalBiconditional
            .fold([f1, f2])
            .unwrap_or_else(|[f1, f2]| Self::equivalent(f1, f2))
    }
}
