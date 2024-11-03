use std::{any::Any, fmt::Debug, ops::Deref};

use dyn_clone::{clone_trait_object, DynClone};

use crate::{
    connective::Connective,
    utils::{
        upcast::{Upcast, UpcastFrom},
        Zst,
    },
};

#[derive(Debug, Clone)]
/// Wrapper for dynamic [`Connective`] with more traits enabled for usability.
pub struct DynConnective {
    inner: Box<dyn UsableConnective<2>>,
}

impl DynConnective {
    /// Create a [`DynOperator`] with a [`Connective<2>`].
    pub fn new<C>() -> Self
    where
        C: Connective<2> + Zst + Debug + Copy + 'static,
    {
        #[allow(path_statements)]
        {
            C::ASSERT_ZST;
        }

        Self {
            inner: Box::new(C::init()),
        }
    }
}

impl Deref for DynConnective {
    type Target = dyn Connective<2>;

    fn deref(&self) -> &Self::Target {
        self.inner.as_ref().up()
    }
}

impl PartialEq for DynConnective {
    fn eq(&self, other: &Self) -> bool {
        (*self.inner).type_id() == (*other.inner).type_id()
    }
}

impl Eq for DynConnective {}

trait UsableConnective<const N: usize>:
    Connective<N> + Upcast<dyn Connective<N>> + Debug + DynClone
{
}

impl<const N: usize, T> UsableConnective<N> for T where T: Connective<N> + Debug + DynClone + 'static
{}

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
}
