use std::ops;

use crate::connective::{functions, Reducible as _};

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

        // break the recursion while using it in the:
        // - `impl Reducible for connective::*`;
        // - `Evaluable::not`.
        //
        // Do not use the `functions::Negation.try_reduce([f])`
        // since it recursively calls the same function again.
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
        let f1 = self.into();
        let f2 = e.into();

        functions::Conjunction
            .try_reduce([f1, f2])
            .unwrap_or_else(|[f1, f2]| Formula::And(Box::new(f1), Box::new(f2)))
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
        let f1 = self.into();
        let f2 = e.into();

        functions::Disjunction
            .try_reduce([f1, f2])
            .unwrap_or_else(|[f1, f2]| Formula::Or(Box::new(f1), Box::new(f2)))
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
        let f1 = self.into();
        let f2 = e.into();

        functions::ExclusiveDisjunction
            .try_reduce([f1, f2])
            .unwrap_or_else(|[f1, f2]| Formula::Xor(Box::new(f1), Box::new(f2)))
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
        let f1 = self.into();
        let f2 = e.into();

        functions::MaterialImplication
            .try_reduce([f1, f2])
            .unwrap_or_else(|[f1, f2]| Formula::Implies(Box::new(f1), Box::new(f2)))
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
        let f1 = self.into();
        let f2 = e.into();

        functions::LogicalBiconditional
            .try_reduce([f1, f2])
            .unwrap_or_else(|[f1, f2]| Formula::Equivalent(Box::new(f1), Box::new(f2)))
    }
}
