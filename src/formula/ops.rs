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

        // try to reduce the formulas into a degenerate case, if possible
        match f {
            Formula::TruthValue(x) => Formula::TruthValue(!x),
            _ => Formula::Not(Box::new(f)),
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

        // TODO: reuse the `Reducible` trait
        // try to reduce the formulas into a single one, if possible
        match (f1, f2) {
            (Formula::TruthValue(x), Formula::TruthValue(y)) => Formula::TruthValue(x && y),
            (Formula::TruthValue(x), f) | (f, Formula::TruthValue(x)) => {
                if x {
                    f
                } else {
                    Formula::contradiction()
                }
            }
            (f1, f2) => Formula::And(Box::new(f1), Box::new(f2)),
        }
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

        // try to reduce the formulas into a single one, if possible
        match (f1, f2) {
            (Formula::TruthValue(x), Formula::TruthValue(y)) => Formula::TruthValue(x || y),
            (Formula::TruthValue(x), f) | (f, Formula::TruthValue(x)) => {
                if x {
                    Formula::tautology()
                } else {
                    f
                }
            }
            (f1, f2) => Formula::Or(Box::new(f1), Box::new(f2)),
        }
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

        // try to reduce the formulas into a single one, if possible
        match (f1, f2) {
            (Formula::TruthValue(x), Formula::TruthValue(y)) => Formula::TruthValue(x ^ y),
            (Formula::TruthValue(x), f) | (f, Formula::TruthValue(x)) => {
                if x {
                    f.not()
                } else {
                    f
                }
            }
            (f1, f2) => Formula::Xor(Box::new(f1), Box::new(f2)),
        }
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

        // try to reduce the formulas into a single one, if possible
        match (f1, f2) {
            (Formula::TruthValue(antecedent), Formula::TruthValue(consequent)) => {
                Formula::TruthValue(!antecedent || consequent)
            }
            (Formula::TruthValue(antecedent), consequent) => {
                if antecedent {
                    consequent
                } else {
                    Formula::tautology()
                }
            }
            (antecedent, Formula::TruthValue(consequent)) => {
                if consequent {
                    Formula::tautology()
                } else {
                    antecedent.not()
                }
            }
            (f1, f2) => Formula::Implies(Box::new(f1), Box::new(f2)),
        }
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

        // try to reduce the formulas into a single one, if possible
        match (f1, f2) {
            (Formula::TruthValue(x), Formula::TruthValue(y)) => Formula::TruthValue(x == y),
            (Formula::TruthValue(x), f) | (f, Formula::TruthValue(x)) => {
                if x {
                    f
                } else {
                    f.not()
                }
            }
            (f1, f2) => Formula::Equivalent(Box::new(f1), Box::new(f2)),
        }
    }
}
