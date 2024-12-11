use std::ops::Not;

/// Represent the intermediate result of boolean operations.
pub trait Evaluable {
    /// The 'partial' evaluation contains
    /// some term that is going to be evaluated but
    /// has no determinedd bool value yet.
    type Partial;

    /// Create a 'terminal' bool result of evaluation.
    fn terminal(value: bool) -> Self;

    /// Create a 'partial' evaluation which contains
    /// some term that is going to be evaluated but not ready yet.
    fn partial(val: Self::Partial) -> Self;

    /// Terminal result equivalent to [`Truth`][super::Truth].
    fn tautology() -> Self
    where
        Self: Sized,
    {
        Self::terminal(true)
    }

    /// Terminal result equivalent to [`Falsity`][super::Falsity].
    fn contradiction() -> Self
    where
        Self: Sized,
    {
        Self::terminal(false)
    }

    /// Convert the [`Evaluable`] into bool result
    /// or return the partial term `Err(T)` instead.
    ///
    /// # Errors
    ///
    /// If the [`Evaluable`] is not terminal, return `Err(T)`.
    fn into_terminal(self) -> Result<bool, Self::Partial>;

    /// Convert the [`Evaluable`] into partial term
    /// or return the terminal `Err(bool)` instead.
    ///
    /// # Errors
    ///
    /// If the [`Evaluable`] is not partial, return `Err(bool)`.
    fn into_partial(self) -> Result<Self::Partial, bool>
    where
        Self: Sized,
    {
        match self.into_terminal() {
            Ok(t) => Err(t),
            Err(p) => Ok(p),
        }
    }

    /// Reverse the evaluation.
    ///
    /// This should be a
    ///
    /// `impl<T: Not<Output = T>, E: Evaluable<T>> Not for E { ... }`
    ///
    /// but the latter cannot be defined because of the 'orphan' rule.
    fn not(self) -> Self
    where
        Self: Sized,
        Self::Partial: Not<Output = Self::Partial>,
    {
        match self.into_terminal() {
            Ok(x) => Self::terminal(!x),

            // Be aware the recursion can arise
            // when `impl Evaluable<T> for T`
            // if the `impl Not for T` defined itself
            // in terms of `Evaluable<T>`.
            //
            // This way you should break the recursion manually:
            // - either with the manual implementation of this method (`Evaluable::not`);
            // - or (better) by providing the `impl Not for T` using
            //   the `T`'s internal structure details rather than relying
            //   on this `Evaluable::not` implementation.
            Err(partial) => Self::partial(!partial),
        }
    }
}
