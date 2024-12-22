use std::ops;

use crate::connective::{functions, Reducible as _};

use super::{
    super::ops::{And, Equivalent, Implies, Or, Xor},
    formula::Formula,
};

impl<T> ops::Not for Formula<T> {
    type Output = Self;

    fn not(self) -> Self::Output {
        // break the recursion while using it in the:
        // - `impl Reducible for connective::*`;
        // - `crate::formula::ops::Not::not`.
        //
        // Do not use the `functions::Negation.try_reduce([f])`
        // since it recursively calls the same function again.

        if let Self::TruthValue(value) = &self {
            Self::TruthValue(!*value)
        } else {
            Self::Not(Box::new(self))
        }
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

impl<T, LHS, RHS> And<RHS, Formula<T>> for LHS
where
    LHS: Into<Formula<T>>,
    RHS: Into<Formula<T>>,
{
    fn and(self, rhs: RHS) -> Formula<T> {
        let f1 = self.into();
        let f2 = rhs.into();

        functions::Conjunction
            .try_reduce([f1, f2])
            .unwrap_or_else(|[f1, f2]| Formula::And(Box::new(f1), Box::new(f2)))
    }
}

impl<T, LHS, RHS> Or<RHS, Formula<T>> for LHS
where
    LHS: Into<Formula<T>>,
    RHS: Into<Formula<T>>,
{
    fn or(self, rhs: RHS) -> Formula<T> {
        let f1 = self.into();
        let f2 = rhs.into();

        functions::Disjunction
            .try_reduce([f1, f2])
            .unwrap_or_else(|[f1, f2]| Formula::Or(Box::new(f1), Box::new(f2)))
    }
}

impl<T, LHS, RHS> Xor<RHS, Formula<T>> for LHS
where
    LHS: Into<Formula<T>>,
    RHS: Into<Formula<T>>,
{
    fn xor(self, rhs: RHS) -> Formula<T> {
        let f1 = self.into();
        let f2 = rhs.into();

        functions::ExclusiveDisjunction
            .try_reduce([f1, f2])
            .unwrap_or_else(|[f1, f2]| Formula::Xor(Box::new(f1), Box::new(f2)))
    }
}

impl<T, LHS, RHS> Implies<RHS, Formula<T>> for LHS
where
    LHS: Into<Formula<T>>,
    RHS: Into<Formula<T>>,
{
    fn implies(self, rhs: RHS) -> Formula<T> {
        let f1 = self.into();
        let f2 = rhs.into();

        functions::MaterialImplication
            .try_reduce([f1, f2])
            .unwrap_or_else(|[f1, f2]| Formula::Implies(Box::new(f1), Box::new(f2)))
    }
}

impl<T, LHS, RHS> Equivalent<RHS, Formula<T>> for LHS
where
    LHS: Into<Formula<T>>,
    RHS: Into<Formula<T>>,
{
    fn equivalent(self, rhs: RHS) -> Formula<T> {
        let f1 = self.into();
        let f2 = rhs.into();

        functions::LogicalBiconditional
            .try_reduce([f1, f2])
            .unwrap_or_else(|[f1, f2]| Formula::Equivalent(Box::new(f1), Box::new(f2)))
    }
}
