use crate::{connective::Evaluable, utils::zst::Void};

/// An atomic entity with no deeper propositional structure.
///
/// For propositional logic, a [propositional variable][super::super::Variable]
/// is often more briefly referred to as an [atomic formula][Atom],
/// but, more precisely, a [propositional variable][super::super::Variable]
/// is not an atomic formula but a formal expression that denotes an atomic formula.
pub trait Atom {}

/// The [truth value](https://en.wikipedia.org/wiki/Truth_value)
/// associated with the [`Atom`] variable
/// (isomorphic to `Option<bool>`).
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub enum Assignment<T = bool> {
    #[default]
    /// No value yet assigned.
    Unknown,

    /// The value assigned.
    Value(T),
}

impl<T> From<Option<T>> for Assignment<T> {
    fn from(value: Option<T>) -> Self {
        value.map_or(Self::Unknown, Self::Value)
    }
}

impl<T> From<Assignment<T>> for Option<T> {
    fn from(value: Assignment<T>) -> Self {
        match value {
            Assignment::Value(x) => Some(x),
            Assignment::Unknown => None,
        }
    }
}

impl<T> Assignment<T> {
    /// Get the [truth value](https://en.wikipedia.org/wiki/Truth_value)
    /// if there is any.
    pub const fn get(&self) -> Option<&T> {
        match self {
            Self::Value(x) => Some(x),
            Self::Unknown => None,
        }
    }
}

impl Evaluable for Assignment<bool> {
    type Partial = ();

    fn terminal(value: bool) -> Self {
        Self::Value(value)
    }

    fn is_tautology(&self) -> bool {
        matches!(self, Self::Value(x) if *x)
    }

    fn is_contradiction(&self) -> bool {
        matches!(self, Self::Value(x) if !*x)
    }

    fn partial(_empty: ()) -> Self {
        Self::Unknown
    }

    fn into_terminal(self) -> Result<bool, ()> {
        self.get().copied().ok_or(())
    }
}

impl Evaluable for bool {
    type Partial = Void;

    fn terminal(value: bool) -> Self {
        value
    }

    fn is_tautology(&self) -> bool {
        *self
    }

    fn is_contradiction(&self) -> bool {
        !*self
    }

    fn partial(_: Void) -> Self {
        unreachable!("This function cannot be called")
    }

    fn into_terminal(self) -> Result<bool, Void> {
        Ok(self)
    }
}
