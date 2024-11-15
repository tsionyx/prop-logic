use std::fmt;

use crate::utils::zst::Void;

use super::atom::Atom;

type VarId = u64;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
/// A propositional variable is a
/// formal expression that denotes an [atomic formula][Atom].
///
/// It is a basic building block
/// of the propositional calculus.  
///
/// <https://en.wikipedia.org/wiki/Propositional_variable>
pub struct Variable<T> {
    id: VarId,
    extra: Option<T>,
}

/// The most primitive instantiation of the [`Variable`]
/// without any additional info attached
/// optimized for memory usage.
pub type Var = Variable<Void>;

impl<T> Variable<T> {
    /// Create a new [`Variable`] without any extra data associated with it.
    pub const fn new(id: VarId) -> Self {
        Self { id, extra: None }
    }

    /// Create a new [`Variable`]
    /// associating some extra data with it.
    pub const fn with_data(id: VarId, data: T) -> Self {
        Self {
            id,
            extra: Some(data),
        }
    }
}

impl<T> Atom for Variable<T> {}

impl<T> fmt::Display for Variable<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(extra) = &self.extra {
            if f.alternate() {
                write!(f, "{extra} (id={})", self.id)
            } else {
                write!(f, "{extra}")
            }
        } else {
            write!(f, "{}", self.id)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn size_var_contains_no_label() {
        use std::mem::*;

        let var = Var::new(12);
        assert_eq!(size_of_val(&var), size_of::<VarId>());
    }
}
