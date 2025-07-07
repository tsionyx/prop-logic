use std::fmt;

use crate::utils::zst::Void;

type VarId = u64;

#[derive(Debug, PartialEq, Eq, Copy, Clone, PartialOrd, Ord, Hash)]
/// A [propositional variable][Variable] is a
/// formal expression that denotes an atomic formula.
///
/// Despite of the fact a [`Formula`][crate::Formula]
/// could be created using arbitrary type as a variable,
/// this wrapper type allows to assign unified IDs
/// and provides some more useful methods:
///
/// - `std::ops::Neg` to create a [`Literal`][crate::Literal];
/// - assign auto-incremented IDs for all atoms in a [`Formula`][crate::Formula];
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

    /// Get an ID associated with the [`Variable`].
    pub const fn id(&self) -> VarId {
        self.id
    }

    /// Get extra payload data associated with the [`Variable`].
    pub const fn extra(&self) -> Option<&T> {
        self.extra.as_ref()
    }
}

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

mod impls {
    use super::{
        super::{Formula, Literal},
        Var, Variable,
    };

    impl<T> std::ops::Not for Variable<T> {
        type Output = Literal<Self>;

        fn not(self) -> Self::Output {
            Literal::Neg(self)
        }
    }

    impl<T> Variable<T> {
        /// Create a [`Formula`] from the given [`Variable`].
        pub const fn atomize(self) -> Formula<Self> {
            Formula::atom(self)
        }
    }

    impl<T: PartialEq + Clone> From<Formula<T>> for Formula<Variable<T>> {
        fn from(f: Formula<T>) -> Self {
            // build a map of `ID` -> `VAR` first
            let vars: Vec<_> = f
                .variables()
                .into_iter()
                .enumerate()
                .map(|(id, var)| {
                    let id = id
                        .try_into()
                        .expect("Overflowed the VarId type for the number of Formula's variables");

                    // need to clone to ensure no reference to original `Formula`
                    // left when we reach the `Formula::map` further
                    Variable::with_data(id, var.clone())
                })
                .collect();

            f.map(|x| {
                let var_id = vars
                    .iter()
                    .find_map(|var| (var.extra() == Some(&x)).then_some(var.id()))
                    .expect("Should exist the variable with ID for every original variable");
                Variable::with_data(var_id, x)
            })
        }
    }

    impl<T> Formula<Variable<T>> {
        /// Clear the payload data from the atoms' [`Variable`].
        pub fn into_var_id(self) -> Formula<Var> {
            self.map(|x| Var::new(x.id()))
        }

        #[cfg(test)]
        #[expect(dead_code)]
        /// Clear the variable ID from the atoms' [`Variable`],
        /// by leaving only extra variable's [payload][Variable::extra].
        ///
        /// # Panics
        ///
        /// The function will panic if any variable has no defined payload.
        pub(crate) fn into_payload_var(self) -> Formula<T> {
            self.map(|x| x.extra.unwrap())
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
