/// An atomic entity with no deeper propositional structure.
///
/// For propositional logic, a [propositional variable][Variable]
/// is often more briefly referred to as an [atomic formula][Atom],
/// but, more precisely, a [propositional variable][Variable]
/// is not an atomic formula but a formal expression that denotes an atomic formula.
pub trait Atom {}

#[derive(Debug, Default, Copy, Clone)]
/// The concrete [truth value](https://en.wikipedia.org/wiki/Truth_value)
/// associated with the [`Atom`].
///
/// It can be no value assigned.
pub struct AtomValue {
    pub(crate) value: Option<bool>,
}

impl AtomValue {
    /// Get the [truth value](https://en.wikipedia.org/wiki/Truth_value)
    /// if there is any.
    pub const fn get(&self) -> Option<bool> {
        self.value
    }
}
