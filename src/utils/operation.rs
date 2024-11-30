use std::{fmt, ops::Deref};

/// Represents the most general form of
/// [homogeneous operation](https://en.wikipedia.org/wiki/Binary_operation).
///
/// It also works as a workaround emulating `impl Fn` behaviour
/// by allowing to 'call' it through [`Deref`]-ing.
pub struct Operation<const ARITY: usize, T> {
    func: Box<dyn Fn([T; ARITY]) -> T>,
}

impl<const ARITY: usize, T> fmt::Debug for Operation<ARITY, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{ARITY}-ary Operation")
    }
}

impl<const ARITY: usize, T> Operation<ARITY, T> {
    /// Create an [`Operator`] by wrapping the given function.
    pub const fn new(func: Box<dyn Fn([T; ARITY]) -> T>) -> Self {
        Self { func }
    }
}

impl<const ARITY: usize, T> Deref for Operation<ARITY, T>
where
    T: 'static,
{
    type Target = dyn Fn([T; ARITY]) -> T;

    fn deref(&self) -> &Self::Target {
        &self.func
    }
}
