use std::{borrow::Borrow, collections::HashMap as Map, hash::Hash};

use super::atom::Assignment;

#[derive(Debug)]
/// Mapping the [`Atoms`][super::atom::Atom]-s of a `Formula`
/// to one of the [truth values](https://en.wikipedia.org/wiki/Truth_value).
///
/// <https://en.wikipedia.org/wiki/Valuation_(logic)>
pub struct Valuation<T> {
    values: Map<T, Assignment>,
}

impl<T> Default for Valuation<T> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<T> Valuation<T> {
    /// Construct an empty `Valuation`.
    pub fn empty() -> Self {
        Self { values: Map::new() }
    }
}

impl<T> Valuation<T>
where
    T: Eq + Hash,
{
    /// Retrieve a truth value of a specific [`Atom`][super::atom::Atom] if any.
    pub fn get_assignment<Q>(&self, key: &Q) -> Option<bool>
    where
        T: Borrow<Q>,
        Q: Eq + Hash,
    {
        self.values.get(key).and_then(Assignment::get).copied()
    }

    /// Set a truth value to a specific [`Atom`][super::atom::Atom].
    pub fn assign(&mut self, key: T, value: bool) {
        let _previous_value = self.values.insert(key, Assignment::Value(value));
    }
}

impl<T> FromIterator<(T, bool)> for Valuation<T>
where
    T: Eq + Hash,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (T, bool)>,
    {
        let values = iter
            .into_iter()
            .map(|(key, value)| (key, Assignment::Value(value)));
        Self {
            values: values.collect(),
        }
    }
}
