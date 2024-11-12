use std::{any::Any, fmt::Debug, ops::Deref};

use dyn_clone::{clone_trait_object, DynClone};

use crate::{
    connective::{BoolFn, Connective, Prioritized},
    utils::{
        upcast::{Upcast, UpcastFrom},
        Zst,
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
/// Wrapper for [`Connective`] of ARITY from {0, 1, 2} with more traits enabled for usability.
pub enum AnyConnective<OPERAND> {
    /// Nullary [`Connective`].
    Nullary(DynConnective<0>),
    /// Unary [`Connective`].
    Unary {
        /// The unary [`Connective`].
        operator: DynConnective<1>,
        /// The single operand.
        operand: OPERAND,
    },
    /// Binary [`Connective`].
    Binary {
        /// The binary [`Connective`].
        operator: DynConnective<2>,
        /// Two operands for a [`Connective`].
        operands: (OPERAND, OPERAND),
    },
}

impl<OPERAND> AnyConnective<OPERAND> {
    /// Create a [`DynConnective`] with a [`Connective<0>`].
    pub fn new_0<C>() -> Self
    where
        C: Connective<0> + Prioritized + Zst + Debug + Copy + 'static,
    {
        Self::Nullary(DynConnective::new::<C>())
    }

    /// Create a [`DynConnective`] with a [`Connective<1>`].
    pub fn new_1<C>(operand: OPERAND) -> Self
    where
        C: Connective<1> + Prioritized + Zst + Debug + Copy + 'static,
    {
        Self::Unary {
            operator: DynConnective::new::<C>(),
            operand,
        }
    }

    /// Create a [`DynConnective`] with a [`Connective<2>`].
    pub fn new_2<C>(operands: (OPERAND, OPERAND)) -> Self
    where
        C: Connective<2> + Prioritized + Zst + Debug + Copy + 'static,
    {
        Self::Binary {
            operator: DynConnective::new::<C>(),
            operands,
        }
    }

    /// Forget the operands and return 'only-operator' version of [`AnyConnective`].
    pub fn clear_operands(&self) -> AnyConnective<()> {
        match self {
            Self::Nullary(operator) => AnyConnective::Nullary(operator.clone()),
            Self::Unary { operator, .. } => AnyConnective::Unary {
                operator: operator.clone(),
                operand: (),
            },
            Self::Binary { operator, .. } => AnyConnective::Binary {
                operator: operator.clone(),
                operands: ((), ()),
            },
        }
    }

    /// The 'reference' version of [`AnyConnective`].
    pub fn as_ref<U: ?Sized>(&self) -> AnyConnective<&U>
    where
        OPERAND: AsRef<U>,
    {
        match self {
            Self::Nullary(operator) => AnyConnective::Nullary(operator.clone()),
            Self::Unary { operator, operand } => AnyConnective::Unary {
                operator: operator.clone(),
                operand: operand.as_ref(),
            },
            Self::Binary { operator, operands } => AnyConnective::Binary {
                operator: operator.clone(),
                operands: (operands.0.as_ref(), operands.1.as_ref()),
            },
        }
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

impl<const ARITY: usize> BoolFn<ARITY> for DynConnective<ARITY> {
    fn eval(&self, values: [bool; ARITY]) -> bool {
        self.0.eval(values)
    }
}

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

    impl<OPERAND> Prioritized for AnyConnective<OPERAND> {
        fn priority(&self) -> Priority {
            match self {
                Self::Nullary(operator) => operator.priority(),
                Self::Unary { operator, .. } => operator.priority(),
                Self::Binary { operator, .. } => operator.priority(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::connective::{Conjunction, Disjunction};

    use super::*;

    #[test]
    fn dyn_equality() {
        let x = DynConnective::new::<Conjunction>();
        let y = DynConnective::new::<Disjunction>();
        let z = DynConnective::new::<Conjunction>();

        assert_ne!(x, y);
        assert_eq!(x, z);
    }

    #[test]
    fn any_equality_with_clearing_operands() {
        let x = AnyConnective::Binary {
            operator: DynConnective::new::<Conjunction>(),
            operands: ((), ()),
        };
        let y = AnyConnective::Binary {
            operator: DynConnective::new::<Disjunction>(),
            operands: ((), ()),
        };
        let z = AnyConnective::Binary {
            operator: DynConnective::new::<Conjunction>(),
            operands: ((), ()),
        };

        assert_ne!(x, y);
        assert_eq!(x, z);

        let x = x.clear_operands();
        let y = y.clear_operands();
        let z = z.clear_operands();

        assert_ne!(x, y);
        assert_eq!(x, z);
    }

    #[test]
    fn any_equality_with_ref() {
        let x = AnyConnective::Binary {
            operator: DynConnective::new::<Conjunction>(),
            operands: ("a".to_string(), "b".to_string()),
        };
        let y = AnyConnective::Binary {
            operator: DynConnective::new::<Disjunction>(),
            operands: ("a".to_string(), "b".to_string()),
        };
        let z = AnyConnective::Binary {
            operator: DynConnective::new::<Conjunction>(),
            operands: ("a".to_string(), "b".to_string()),
        };

        assert_ne!(x, y);
        assert_eq!(x, z);

        let x_ref = x.as_ref::<str>();
        let y_ref = y.as_ref();
        let z_ref = z.as_ref();

        assert_ne!(x_ref, y_ref);
        assert_eq!(x_ref, z_ref);
    }
}
