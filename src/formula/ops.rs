use crate::connective::Evaluable;

/// The logical negation operator.
pub trait Not<Target> {
    /// Reverse the evaluation.
    ///
    /// This should be a
    ///
    /// `impl<T: Not<Output = T>, E: Evaluable<T>> Not for E { ... }`
    ///
    /// but the latter cannot be defined because of the 'orphan' rule.
    fn not(self) -> Target;
}

impl<T, E> Not<E> for T
where
    T: Into<E>,
    E: Evaluable,
    E::Partial: std::ops::Not<Output = E::Partial>,
{
    fn not(self) -> E {
        let e = self.into();
        match e.into_terminal() {
            Ok(x) => E::terminal(!x),

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
            Err(partial) => E::partial(!partial),
        }
    }
}

/// The logical conjunction operator.
pub trait And<RHS: Into<Target>, Target> {
    /// Performs the logical conjunction.
    fn and(self, rhs: RHS) -> Target;
}

/// The logical disjunction operator.
pub trait Or<RHS: Into<Target>, Target> {
    /// Performs the logical disjunction.
    fn or(self, rhs: RHS) -> Target;
}

/// The logical exclusive disjunction (XOR) operator.
pub trait Xor<RHS: Into<Target>, Target> {
    /// Performs the logical exclusive disjunction (XOR).
    fn xor(self, rhs: RHS) -> Target;
}

/// The logical implication operator.
pub trait Implies<RHS: Into<Target>, Target> {
    /// Performs the logical implication.
    fn implies(self, rhs: RHS) -> Target;
}

/// The logical equivalence operator.
pub trait Equivalent<RHS: Into<Target>, Target> {
    /// Performs the logical equivalence.
    fn equivalent(self, rhs: RHS) -> Target;
}
