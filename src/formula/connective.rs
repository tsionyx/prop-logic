use std::fmt::Debug;

use derive_where::derive_where;

use crate::{
    connective::{BoolFn, Connective, Evaluation, FormulaComposer, Prioritized, Reducible},
    utils::{upcast::Upcast as _, Zst},
};

use super::formula::Formula;

use self::usable::UsableConnective;

#[derive(Debug)]
#[derive_where(Clone; OPERAND: Clone)]
#[derive_where(PartialEq; OPERAND: PartialEq, Atom: 'static)]
/// [`Connective`] + [`FormulaComposer`] of ARITY from {0, 1, 2} along with their operands.
pub enum AnyConnective<OPERAND, Atom> {
    /// Nullary [`Connective`].
    Nullary(DynConnective<0, Atom>),
    /// Unary [`Connective`].
    Unary {
        /// The unary [`Connective`].
        operator: DynConnective<1, Atom>,
        /// The single operand.
        operand: OPERAND,
    },
    /// Binary [`Connective`].
    Binary {
        /// The binary [`Connective`].
        operator: DynConnective<2, Atom>,
        /// Two operands for a [`Connective`].
        operands: (OPERAND, OPERAND),
    },
}

impl<OPERAND, Atom> Eq for AnyConnective<OPERAND, Atom>
where
    OPERAND: PartialEq,
    Atom: 'static,
{
}

impl<OPERAND, Atom> AnyConnective<OPERAND, Atom> {
    /// Create a [`DynConnective`] with a [`Connective<0>`].
    pub fn new_0<C>(connective: C) -> Self
    where
        C: Connective<0>
            + FormulaComposer<0, Atom>
            + Prioritized
            + Debug
            + Clone
            + PartialEq
            + 'static,
    {
        Self::Nullary(DynConnective::new(connective))
    }

    /// Create a [`DynConnective`] with a [`Connective<1>`].
    pub fn new_1<C>(connective: C, operand: OPERAND) -> Self
    where
        C: Connective<1>
            + FormulaComposer<1, Atom>
            + Prioritized
            + Debug
            + Clone
            + PartialEq
            + 'static,
    {
        Self::Unary {
            operator: DynConnective::new(connective),
            operand,
        }
    }

    /// Create a [`DynConnective`] with a [`Connective<2>`].
    pub fn new_2<C>(connective: C, operands: (OPERAND, OPERAND)) -> Self
    where
        C: Connective<2>
            + FormulaComposer<2, Atom>
            + Prioritized
            + Debug
            + Clone
            + PartialEq
            + 'static,
    {
        Self::Binary {
            operator: DynConnective::new(connective),
            operands,
        }
    }

    /// Forget the operands and return 'only-operator' version of [`AnyConnective`].
    pub fn clear_operands(&self) -> AnyConnective<(), Atom> {
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
    pub fn as_ref<U: ?Sized>(&self) -> AnyConnective<&U, Atom>
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

#[derive(Debug)]
#[derive_where(Clone)]
/// Wrapper for dynamic [`Connective`] and [`FormulaComposer`].
pub struct DynConnective<const ARITY: usize, Atom>(Box<dyn UsableConnective<ARITY, Atom>>);

impl<const ARITY: usize, Atom> DynConnective<ARITY, Atom> {
    /// Create a [`DynConnective`] with a [`Connective<0>`].
    pub fn new<C>(connective: C) -> Self
    where
        C: Connective<ARITY>
            + FormulaComposer<ARITY, Atom>
            + Prioritized
            + Debug
            + Clone
            + PartialEq
            + 'static,
    {
        #[allow(path_statements, clippy::no_effect)]
        {
            C::ASSERT_ZST; // `Sized` ensured by `Clone`
        }

        Self(Box::new(connective))
    }
}

impl<'a, const ARITY: usize, Atom: 'a> AsRef<dyn Connective<ARITY> + 'a>
    for DynConnective<ARITY, Atom>
{
    fn as_ref(&self) -> &(dyn Connective<ARITY> + 'a) {
        self.0.up()
    }
}

impl<const ARITY: usize, Atom: 'static> PartialEq for DynConnective<ARITY, Atom> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == &other.0
    }
}

impl<const ARITY: usize, Atom: 'static> Eq for DynConnective<ARITY, Atom> {}

impl<const ARITY: usize, Atom> BoolFn<ARITY> for DynConnective<ARITY, Atom> {
    fn eval(&self, values: [bool; ARITY]) -> bool {
        self.0.eval(values)
    }
}

impl<const ARITY: usize, T> Reducible<ARITY, Formula<T>> for DynConnective<ARITY, T> {
    fn try_reduce(
        &self,
        values: [Evaluation<Formula<T>>; ARITY],
    ) -> Option<Evaluation<Formula<T>>> {
        self.0.try_reduce(values)
    }
}

impl<const ARITY: usize, T> FormulaComposer<ARITY, T> for DynConnective<ARITY, T> {
    fn compose(&self, formulas: [Formula<T>; ARITY]) -> Formula<T> {
        self.0.compose(formulas)
    }
}

mod usable {
    use std::fmt::Debug;

    use dyn_clone::{clone_trait_object, DynClone};

    use crate::utils::dyn_eq::DynCompare;

    use super::{Connective, FormulaComposer, Prioritized};

    /// [`Connective`]'s marker subtrait to be used in [`DynConnective`][super::DynConnective].
    pub(super) trait UsableConnective<const N: usize, Atom>:
        Connective<N> + FormulaComposer<N, Atom> + Prioritized + Debug + DynClone + DynCompare
    {
    }

    impl<const N: usize, Atom, T> UsableConnective<N, Atom> for T where
        T: Connective<N> + FormulaComposer<N, Atom> + Prioritized + Debug + DynClone + DynCompare
    {
    }

    clone_trait_object!(<const N: usize, Atom> UsableConnective<N, Atom>);

    // Need `Atom: 'static` to ensure conversion to `AsDynCompare`
    // and the `AsDynCompare` requires `Any`, that in turn requires `'static`.
    impl<const N: usize, Atom: 'static> PartialEq<&Self> for Box<dyn UsableConnective<N, Atom> + '_> {
        fn eq(&self, other: &&Self) -> bool {
            self.as_dyn_compare() == other.as_dyn_compare()
        }
    }
}

mod impls {
    use super::{AnyConnective, DynConnective};

    #[allow(clippy::wildcard_imports)]
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

    impl<const ARITY: usize, Atom> Prioritized for DynConnective<ARITY, Atom> {
        fn priority(&self) -> Priority {
            self.0.priority()
        }
    }

    impl<OPERAND, Atom> Prioritized for AnyConnective<OPERAND, Atom> {
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
        let x = DynConnective::<2, ()>::new(Conjunction);
        let y = DynConnective::new(Disjunction);
        let z = DynConnective::new(Conjunction);

        assert_ne!(x, y);
        assert_eq!(x, z);
    }

    #[test]
    fn any_equality_with_clearing_operands() {
        let x = AnyConnective::<_, ()>::Binary {
            operator: DynConnective::new(Conjunction),
            operands: ((), ()),
        };
        let y = AnyConnective::<_, ()>::Binary {
            operator: DynConnective::new(Disjunction),
            operands: ((), ()),
        };
        let z = AnyConnective::<_, ()>::Binary {
            operator: DynConnective::new(Conjunction),
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
        let x = AnyConnective::<_, ()>::Binary {
            operator: DynConnective::new(Conjunction),
            operands: ("a".to_string(), "b".to_string()),
        };
        let y = AnyConnective::<_, ()>::Binary {
            operator: DynConnective::new(Disjunction),
            operands: ("a".to_string(), "b".to_string()),
        };
        let z = AnyConnective::<_, ()>::Binary {
            operator: DynConnective::new(Conjunction),
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
