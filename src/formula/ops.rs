use std::ops;

use super::formula::Formula;

impl<T> ops::Not for Formula<T> {
    type Output = Self;
    fn not(self) -> Self::Output {
        Not::not(self)
    }
}

impl<RHS, T> ops::BitAnd<RHS> for Formula<T>
where
    RHS: Into<Self>,
{
    type Output = Self;
    fn bitand(self, e: RHS) -> Self::Output {
        self.and(e.into())
    }
}

impl<RHS, T> ops::BitOr<RHS> for Formula<T>
where
    RHS: Into<Self>,
{
    type Output = Self;
    fn bitor(self, e: RHS) -> Self::Output {
        self.or(e.into())
    }
}

impl<RHS, T> ops::BitXor<RHS> for Formula<T>
where
    RHS: Into<Self>,
{
    type Output = Self;
    fn bitxor(self, e: RHS) -> Self::Output {
        self.xor(e.into())
    }
}

/// The logical negation operator.
pub trait Not<T> {
    /// Performs the logical negation.
    fn not(self) -> Formula<T>;
}

impl<T, F> Not<T> for F
where
    F: Into<Formula<T>>,
{
    fn not(self) -> Formula<T> {
        let f = self.into();

        // break the recursion while using it in the `Evaluable::not`
        // since it recursively calls the same function again
        match f {
            Formula::TruthValue(value) => Formula::TruthValue(!value),
            f => Formula::Not(Box::new(f)),
        }
    }
}

/// The logical conjunction operator.
pub trait And<T, RHS> {
    /// Performs the logical conjunction.
    fn and(self, rhs: RHS) -> Formula<T>;
}

impl<T, LHS, RHS> And<T, RHS> for LHS
where
    RHS: Into<Formula<T>>,
    LHS: Into<Formula<T>>,
{
    fn and(self, e: RHS) -> Formula<T> {
        Formula::And(Box::new(self.into()), Box::new(e.into()))
    }
}

/// The logical disjunction operator.
pub trait Or<T, RHS> {
    /// Performs the logical disjunction.
    fn or(self, rhs: RHS) -> Formula<T>;
}

impl<T, LHS, RHS> Or<T, RHS> for LHS
where
    RHS: Into<Formula<T>>,
    LHS: Into<Formula<T>>,
{
    fn or(self, e: RHS) -> Formula<T> {
        Formula::Or(Box::new(self.into()), Box::new(e.into()))
    }
}

/// The logical exclusive disjunction (XOR) operator.
pub trait Xor<T, RHS> {
    /// Performs the logical exclusive disjunction (XOR).
    fn xor(self, rhs: RHS) -> Formula<T>;
}

impl<T, LHS, RHS> Xor<T, RHS> for LHS
where
    RHS: Into<Formula<T>>,
    LHS: Into<Formula<T>>,
{
    fn xor(self, e: RHS) -> Formula<T> {
        Formula::Xor(Box::new(self.into()), Box::new(e.into()))
    }
}

/// The logical implication operator.
pub trait Implies<T, RHS> {
    /// Performs the logical implication.
    fn implies(self, rhs: RHS) -> Formula<T>;
}

impl<T, LHS, RHS> Implies<T, RHS> for LHS
where
    RHS: Into<Formula<T>>,
    LHS: Into<Formula<T>>,
{
    fn implies(self, e: RHS) -> Formula<T> {
        Formula::Implies(Box::new(self.into()), Box::new(e.into()))
    }
}

/// The logical equivalence operator.
pub trait Equivalent<T, RHS> {
    /// Performs the logical equivalence.
    fn equivalent(self, rhs: RHS) -> Formula<T>;
}

impl<T, LHS, RHS> Equivalent<T, RHS> for LHS
where
    RHS: Into<Formula<T>>,
    LHS: Into<Formula<T>>,
{
    fn equivalent(self, e: RHS) -> Formula<T> {
        Formula::Equivalent(Box::new(self.into()), Box::new(e.into()))
    }
}
