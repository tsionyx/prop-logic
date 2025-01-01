use crate::connective::{Evaluable, TruthFn};

/// The logical negation operator.
pub trait Not {
    /// Reverse the evaluation.
    ///
    /// This should be a
    ///
    /// `impl<T: Not<Output = T>, E: Evaluable<T>> Not for E { ... }`
    ///
    /// but the latter cannot be defined because of the 'orphan' rule.
    fn not(self) -> Self;
}

impl<E> Not for E
where
    E: Evaluable + std::ops::Not<Output = E>,
{
    fn not(self) -> E {
        // Be aware the recursion can arise
        // when `impl Evaluable<T> for T`
        // if the `impl std::ops::Not for T` defined itself
        // in terms of `Evaluable<T>`.
        //
        // This way you should break the recursion manually:
        // - either with the manual implementation of this method (`Not::not`)
        //   (**CANNOT BE USED** without _specialization_ feature because of this _impl_ itself);
        // - or (better) by providing the `impl std::ops::Not for T` using
        //   the `T`'s internal structure details rather than relying
        //   on this `Not::not` implementation.
        crate::connective::Negation.compose([self])
    }
}

/// The logical conjunction operator.
pub trait And<Rhs: Into<Self>>: Sized {
    /// Performs the logical conjunction.
    fn and(self, rhs: Rhs) -> Self;
}

impl<E, Rhs> And<Rhs> for E
where
    Rhs: Into<E>,
    E: Evaluable + std::ops::BitAnd<Output = E>,
{
    fn and(self, rhs: Rhs) -> Self {
        let f1 = self;
        let f2 = rhs.into();

        crate::connective::Conjunction.compose([f1, f2])
    }
}

/// The logical disjunction operator.
pub trait Or<Rhs: Into<Self>>: Sized {
    /// Performs the logical disjunction.
    fn or(self, rhs: Rhs) -> Self;
}

impl<E, Rhs> Or<Rhs> for E
where
    Rhs: Into<E>,
    E: Evaluable + std::ops::BitOr<Output = E>,
{
    fn or(self, rhs: Rhs) -> Self {
        let f1 = self;
        let f2 = rhs.into();

        crate::connective::Disjunction.compose([f1, f2])
    }
}

/// The logical exclusive disjunction (XOR) operator.
pub trait Xor<Rhs: Into<Self>>: Sized {
    /// Performs the logical exclusive disjunction (XOR).
    fn xor(self, rhs: Rhs) -> Self;
}

impl<E, Rhs> Xor<Rhs> for E
where
    Rhs: Into<E>,
    E: Evaluable + std::ops::Not<Output = E> + std::ops::BitXor<Output = E>,
{
    fn xor(self, rhs: Rhs) -> Self {
        let f1 = self;
        let f2 = rhs.into();

        crate::connective::ExclusiveDisjunction.compose([f1, f2])
    }
}

/// The logical implication operator.
pub trait Implies<Rhs: Into<Self>>: Sized {
    /// Performs the logical implication.
    fn implies(self, rhs: Rhs) -> Self;
}

/// The logical equivalence operator.
pub trait Equivalent<Rhs: Into<Self>>: Sized {
    /// Performs the logical equivalence.
    fn equivalent(self, rhs: Rhs) -> Self;
}
