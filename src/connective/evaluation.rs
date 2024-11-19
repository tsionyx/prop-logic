use std::ops::Not;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Represent the intermediate result of boolean operations.
pub enum Evaluation<T> {
    /// Contains some term that is going to be evaluated.
    Partial(T),
    /// Resolved bool value.
    Terminal(bool),
}

impl<T> Evaluation<T> {
    /// Terminal result equivalent to [`Truth`][super::Truth].
    pub const fn tautology() -> Self {
        Self::Terminal(true)
    }

    /// Terminal result equivalent to [`Falsity`][super::Falsity].
    pub const fn contradiction() -> Self {
        Self::Terminal(false)
    }
}

impl<T> Not for Evaluation<T>
where
    T: Not<Output = T>,
{
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Partial(x) => Self::Partial(!x),
            Self::Terminal(x) => Self::Terminal(!x),
        }
    }
}
