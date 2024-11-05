use std::{any::Any, fmt::Debug, ops::Deref};

use dyn_clone::{clone_trait_object, DynClone};

use crate::{
    connective::{Connective, Prioritized},
    utils::{
        upcast::{Upcast, UpcastFrom},
        Zst,
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
/// Wrapper for [`Connective`] of ARITY from {0, 1, 2} with more traits enabled for usability.
pub enum AnyConnective {
    /// Nullary [`Connective`].
    Nullary(DynConnective<0>),
    /// Unary [`Connective`].
    Unary(DynConnective<1>),
    /// Binary [`Connective`].
    Binary(DynConnective<2>),
}

impl AnyConnective {
    /// Create a [`DynConnective`] with a [`Connective<0>`].
    pub fn new_0<C>() -> Self
    where
        C: Connective<0> + Prioritized + Zst + Debug + Copy + 'static,
    {
        Self::Nullary(DynConnective::new::<C>())
    }

    /// Create a [`DynConnective`] with a [`Connective<1>`].
    pub fn new_1<C>() -> Self
    where
        C: Connective<1> + Prioritized + Zst + Debug + Copy + 'static,
    {
        Self::Unary(DynConnective::new::<C>())
    }

    /// Create a [`DynConnective`] with a [`Connective<2>`].
    pub fn new_2<C>() -> Self
    where
        C: Connective<2> + Prioritized + Zst + Debug + Copy + 'static,
    {
        Self::Binary(DynConnective::new::<C>())
    }
}

#[derive(Debug, Clone)]
/// Wrapper for dynamic [`Connective`] with more traits enabled for usability
pub struct DynConnective<const ARITY: usize>(Box<dyn UsableConnective<ARITY>>);

impl<const ARITY: usize> DynConnective<ARITY> {
    /// Create a [`DynConnective`] with a [`Connective<0>`].
    pub fn new<C>() -> Self
    where
        C: Connective<ARITY> + Prioritized + Zst + Debug + Copy + 'static,
    {
        #[allow(path_statements)]
        {
            C::ASSERT_ZST;
        }

        Self(Box::new(C::init()))
    }
}

impl<const ARITY: usize> Deref for DynConnective<ARITY> {
    type Target = dyn Connective<ARITY>;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref().up()
    }
}

impl<const ARITY: usize> PartialEq for DynConnective<ARITY> {
    fn eq(&self, other: &Self) -> bool {
        // use the `Any` supertrait under the hood
        (*self.0).type_id() == (*other.0).type_id()
    }
}

impl<const ARITY: usize> Eq for DynConnective<ARITY> {}

/// [`Connective`]'s subtrait with enabled usability for dynamic context.
trait UsableConnective<const N: usize>:
    Connective<N> + Prioritized + Any + Upcast<dyn Connective<N>> + Debug + DynClone
{
}

impl<const N: usize, T> UsableConnective<N> for T where
    T: Connective<N> + Prioritized + Any + Debug + DynClone + 'static
{
}

clone_trait_object!(<const N: usize> UsableConnective<N>);

impl<'a, const N: usize, T: Connective<N> + 'a> UpcastFrom<T> for dyn Connective<N> + 'a {
    fn up_from(value: &T) -> &(dyn Connective<N> + 'a) {
        value
    }

    fn up_from_mut(value: &mut T) -> &mut (dyn Connective<N> + 'a) {
        value
    }
}

mod impls {
    use super::*;

    use crate::connective::{functions::*, Prioritized, Priority};

    macro_rules! impl_priority {
        ($($t:ty),+ : $pr:expr) => {
            $(
                impl Prioritized for $t {
                    fn priority(&self) -> Priority {
                        Priority($pr)
                    }
                }
            )+
        };
    }

    // TODO: refine the priorities and maybe introduce some more of them

    // most common operations' priorities
    impl_priority!(Falsity, LogicalIdentity, Truth: 255);
    impl_priority!(Negation: 200);
    impl_priority!(Conjunction: 100);
    impl_priority!(Disjunction, ExclusiveDisjunction: 100);
    impl_priority!(MaterialImplication, LogicalBiconditional: 90);
    impl_priority!(NonConjunction, NonDisjunction: 80);

    impl<const ARITY: usize> Prioritized for DynConnective<ARITY> {
        fn priority(&self) -> Priority {
            self.0.priority()
        }
    }

    impl Prioritized for AnyConnective {
        fn priority(&self) -> Priority {
            match self {
                Self::Nullary(c) => c.priority(),
                Self::Unary(c) => c.priority(),
                Self::Binary(c) => c.priority(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::connective::{Conjunction, Disjunction};

    use super::*;

    #[test]
    fn two_different_binary_are_not_the_same() {
        let x = DynConnective::new::<Conjunction>();
        let y = DynConnective::new::<Disjunction>();
        let z = DynConnective::new::<Conjunction>();

        assert_ne!(x, y);
        assert_eq!(x, z);
    }
}
