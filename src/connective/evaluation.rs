/// Represent the intermediate result of boolean operations.
pub trait Evaluable {
    /// The 'partial' evaluation contains
    /// some term that is going to be evaluated but
    /// has no determined bool value yet.
    type Partial;

    /// Create a 'terminal' bool result of evaluation.
    fn terminal(value: bool) -> Self;

    /// Terminal result equivalent to [`Truth`][super::Truth].
    fn tautology() -> Self
    where
        Self: Sized,
    {
        Self::terminal(true)
    }

    /// Is the `Evaluation` terminal equivalent to [`Truth`][super::Truth].
    fn is_tautology(&self) -> bool;

    /// Terminal result equivalent to [`Falsity`][super::Falsity].
    fn contradiction() -> Self
    where
        Self: Sized,
    {
        Self::terminal(false)
    }

    /// Is the `Evaluation` terminal equivalent to [`Falsity`][super::Falsity].
    fn is_contradiction(&self) -> bool;

    /// Convert the [`Evaluable`] into bool result
    /// or return the partial term `Err(T)` instead.
    ///
    /// # Errors
    ///
    /// If the [`Evaluable`] is not terminal, return `Err(T)`.
    fn into_terminal(self) -> Result<bool, Self::Partial>;

    /// Create a 'partial' evaluation which contains
    /// some term that is going to be evaluated but not ready yet.
    fn partial(val: Self::Partial) -> Self;

    /// Is the `Evaluation` partial
    /// (not reduced to either [`Truth`][super::Truth] nor [`Falsity`][super::Falsity]).
    fn is_partial(&self) -> bool {
        !(self.is_tautology() || self.is_contradiction())
    }

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
}
