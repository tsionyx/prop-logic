use std::ops::Not;

/// Represent the intermediate result of boolean operations.
pub trait Evaluation<T> {
    /// Create a 'terminal' bool result of evaluation.
    fn terminal(value: bool) -> Self;

    /// Create a 'partial' evaluation which contains
    /// some term that is going to be evaluated but not ready yet.
    fn partial(val: T) -> Self;

    /// Terminal result equivalent to [`Truth`][super::Truth].
    fn tautology() -> Self {
        Self::terminal(true)
    }

    /// Terminal result equivalent to [`Falsity`][super::Falsity].
    fn contradiction() -> Self {
        Self::terminal(false)
    }

    /// Convert the [`Evaluation`] into bool result
    /// or return the partial term instead.
    fn into_terminal(self) -> Result<bool, T>;

    /// Convert the [`Evaluation`] into partial term
    /// or return bool terminal result instead.
    fn into_partial(self) -> Result<T, bool>;

    /// Reverse the evaluation.
    ///
    /// This should be a
    ///
    /// `impl<T: Not<Output = T>, E: Evaluation<T>> Not for E { ... }`
    ///
    /// but the latter cannot be defined because of the 'orphan' rule.
    fn not(self) -> Self
    where
        T: Not,
    {
        match self.into_terminal() {
            Ok(x) => Self::terminal(!x),
            Err(partial) => Self::partial(!partial),
        }
    }
}
