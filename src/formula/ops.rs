use crate::connective::Evaluable;

/// The logical negation operation.
pub trait Not: Sized {
    /// Produce a negation of a value.
    fn not(self) -> Self;
}

impl<E> Not for E
where
    Self: Evaluable + std::ops::Not<Output = Self>,
{
    fn not(self) -> Self {
        !self
    }
}

/// The logical conjunction operation.
pub trait And<Rhs: Into<Self> = Self>: Sized {
    /// Produce a conjunction of two values.
    fn and(self, rhs: Rhs) -> Self;
}

impl<E, Rhs> And<Rhs> for E
where
    Rhs: Into<Self>,
    Self: Evaluable + std::ops::BitAnd<Output = Self>,
{
    fn and(self, rhs: Rhs) -> Self {
        self & rhs.into()
    }
}

/// The logical disjunction operation.
pub trait Or<Rhs: Into<Self> = Self>: Sized {
    /// Produce a disjunction of two values.
    fn or(self, rhs: Rhs) -> Self;
}

impl<E, Rhs> Or<Rhs> for E
where
    Rhs: Into<Self>,
    Self: Evaluable + std::ops::BitOr<Output = Self>,
{
    fn or(self, rhs: Rhs) -> Self {
        self | rhs.into()
    }
}

/// The logical exclusive disjunction (XOR) operation.
pub trait Xor<Rhs: Into<Self> = Self>: Sized {
    /// Produce a logical exclusive disjunction (XOR) of two values.
    fn xor(self, rhs: Rhs) -> Self;
}

impl<E, Rhs> Xor<Rhs> for E
where
    Rhs: Into<E>,
    E: Evaluable + std::ops::BitXor<Output = E>,
{
    fn xor(self, rhs: Rhs) -> Self {
        self ^ rhs.into()
    }
}

/// The logical implication operation.
pub trait Implies<Rhs = Self>: Sized {
    /// Produce a logical implication (conditional) from the first to second value.
    fn implies(self, rhs: Rhs) -> Self;
}

impl<Rhs> Implies<Rhs> for bool
where
    Rhs: Into<Self>,
{
    fn implies(self, consequent: Rhs) -> Self {
        if self {
            consequent.into()
        } else {
            true
        }
    }
}

/// The logical equivalence operation.
pub trait Equivalent<Rhs = Self>: Sized {
    /// Produce a logical equivalence of two values.
    fn equivalent(self, rhs: Rhs) -> Self;
}

impl<Rhs> Equivalent<Rhs> for bool
where
    Rhs: Into<Self>,
{
    fn equivalent(self, other: Rhs) -> Self {
        self == other.into()
    }
}
