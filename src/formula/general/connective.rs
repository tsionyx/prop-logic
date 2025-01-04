use std::fmt::Debug;

use derive_where::derive_where;

use crate::{
    connective::{Connective, Prioritized, TruthFn},
    utils::{upcast::Upcast as _, Zst},
};

use super::formula::Formula;

use self::usable::UsableConnective;

#[derive(Debug)]
#[derive_where(Clone; OPERAND: Clone)]
#[derive_where(PartialEq; OPERAND: PartialEq, Atom: 'static)]
#[derive_where(Eq; OPERAND: Eq, Atom: 'static)]
/// [`Connective`] + [`TruthFn`] of ARITY from {0, 1, 2} along with their operands.
pub enum AnyConnective<OPERAND, Atom> {
    /// Nullary [`Connective`].
    Nullary(DynConnective<0, OPERAND, Atom>),
    /// Unary [`Connective`].
    Unary(DynConnective<1, OPERAND, Atom>),
    /// Binary [`Connective`].
    Binary(DynConnective<2, OPERAND, Atom>),
}

impl<OPERAND, Atom> AnyConnective<OPERAND, Atom> {
    /// Create a [`DynConnective`] with a [`Connective<0>`].
    pub fn new_0<C>(connective: C) -> Self
    where
        C: Connective<0>
            + TruthFn<0, Formula<Atom>>
            + Prioritized
            + Debug
            + Clone
            + PartialEq
            + 'static,
    {
        Self::Nullary(DynConnective::new(connective, []))
    }

    /// Create a [`DynConnective`] with a [`Connective<1>`].
    pub fn new_1<C>(connective: C, operand: OPERAND) -> Self
    where
        C: Connective<1>
            + TruthFn<1, Formula<Atom>>
            + Prioritized
            + Debug
            + Clone
            + PartialEq
            + 'static,
    {
        Self::Unary(DynConnective::new(connective, [operand]))
    }

    /// Create a [`DynConnective`] with a [`Connective<2>`].
    pub fn new_2<C>(connective: C, operands: (OPERAND, OPERAND)) -> Self
    where
        C: Connective<2>
            + TruthFn<2, Formula<Atom>>
            + Prioritized
            + Debug
            + Clone
            + PartialEq
            + 'static,
    {
        Self::Binary(DynConnective::new(connective, operands.into()))
    }

    /// Forget the operands and return 'only-operator' version of [`AnyConnective`].
    pub fn clear_operands(&self) -> AnyConnective<(), Atom> {
        match self {
            Self::Nullary(x) => AnyConnective::Nullary(x.clear_operands()),
            Self::Unary(x) => AnyConnective::Unary(x.clear_operands()),
            Self::Binary(x) => AnyConnective::Binary(x.clear_operands()),
        }
    }

    /// The 'reference' version of [`AnyConnective`].
    pub fn as_ref<U: ?Sized>(&self) -> AnyConnective<&U, Atom>
    where
        OPERAND: AsRef<U>,
    {
        match self {
            Self::Nullary(x) => AnyConnective::Nullary(x.as_ref()),
            Self::Unary(x) => AnyConnective::Unary(x.as_ref()),
            Self::Binary(x) => AnyConnective::Binary(x.as_ref()),
        }
    }

    /// Convert to another [`AnyConnective`] by converting its operands.
    pub fn map<F, OperandTarget>(self, f: F) -> AnyConnective<OperandTarget, Atom>
    where
        F: FnMut(OPERAND) -> OperandTarget,
    {
        match self {
            Self::Nullary(x) => AnyConnective::Nullary(x.map(f)),
            Self::Unary(x) => AnyConnective::Unary(x.map(f)),
            Self::Binary(x) => AnyConnective::Binary(x.map(f)),
        }
    }
}

#[derive(Debug)]
#[derive_where(Clone; OPERAND: Clone)]
// requires `Atom: 'static` because of the `UsableConnective`: `DynCompare`: `AsDynCompare`: `Any`: `'static`
#[derive_where(PartialEq; OPERAND: PartialEq, Atom: 'static)]
#[derive_where(Eq; OPERAND: Eq, Atom: 'static)]
/// Wrapper for dynamic [`Connective`] and [`TruthFn`] with operands attached.
pub struct DynConnective<const ARITY: usize, OPERAND, Atom> {
    pub(super) connective: Box<dyn UsableConnective<ARITY, Atom>>,
    pub(super) operands: [OPERAND; ARITY],
}

impl<const ARITY: usize, OPERAND, Atom> DynConnective<ARITY, OPERAND, Atom> {
    /// Create a [`DynConnective`] with a [`Connective<0>`].
    pub fn new<C>(connective: C, operands: [OPERAND; ARITY]) -> Self
    where
        C: Connective<ARITY>
            + TruthFn<ARITY, Formula<Atom>>
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

        Self {
            connective: Box::new(connective),
            operands,
        }
    }

    /// Forget the operands and return 'only-operator' version of [`DynConnective`].
    pub fn clear_operands(&self) -> DynConnective<ARITY, (), Atom> {
        DynConnective {
            connective: self.connective.clone(),
            operands: [(); ARITY],
        }
    }

    /// The 'reference' version of [`DynConnective`].
    pub fn as_ref<U: ?Sized>(&self) -> DynConnective<ARITY, &U, Atom>
    where
        OPERAND: AsRef<U>,
    {
        // TODO: use `each_ref` stabilized in 1.77 to do conversion without allocating `Vec`
        let operands: Vec<_> = self.operands.iter().map(OPERAND::as_ref).collect();
        DynConnective {
            connective: self.connective.clone(),
            operands: operands
                .try_into()
                .unwrap_or_else(|_| unreachable!("the size of ARITY is preserved")),
        }
    }

    /// Convert to another [`DynConnective`] by converting its operands.
    pub fn map<F, OperandTarget>(self, f: F) -> DynConnective<ARITY, OperandTarget, Atom>
    where
        F: FnMut(OPERAND) -> OperandTarget,
    {
        let Self {
            connective,
            operands,
        } = self;

        DynConnective {
            connective,
            operands: operands.map(f),
        }
    }
}

impl<'a, const ARITY: usize, OPERAND, Atom: 'a> AsRef<dyn Connective<ARITY> + 'a>
    for DynConnective<ARITY, OPERAND, Atom>
{
    fn as_ref(&self) -> &(dyn Connective<ARITY> + 'a) {
        self.connective.up()
    }
}

mod usable {
    use std::fmt::Debug;

    use dyn_clone::{clone_trait_object, DynClone};

    use crate::utils::dyn_eq::DynCompare;

    use super::{Connective, Formula, Prioritized, TruthFn};

    /// [`Connective`]'s marker subtrait to be used in [`DynConnective`][super::DynConnective].
    pub(in super::super) trait UsableConnective<const N: usize, Atom>:
        Connective<N> + TruthFn<N, Formula<Atom>> + Prioritized + Debug + DynClone + DynCompare
    {
    }

    impl<const N: usize, Atom, T> UsableConnective<N, Atom> for T where
        T: Connective<N> + TruthFn<N, Formula<Atom>> + Prioritized + Debug + DynClone + DynCompare
    {
    }

    clone_trait_object!(<const N: usize, Atom> UsableConnective<N, Atom>);

    // Need `Atom: 'static` to ensure conversion to `AsDynCompare`
    // and the `AsDynCompare` requires `Any`, that in turn requires `'static`.
    impl<const N: usize, Atom: 'static> PartialEq for Box<dyn UsableConnective<N, Atom> + '_> {
        fn eq(&self, other: &Self) -> bool {
            self.as_dyn_compare() == other.as_dyn_compare()
        }
    }

    impl<const N: usize, Atom: 'static> Eq for Box<dyn UsableConnective<N, Atom> + '_> {}
}

mod impls {
    use super::AnyConnective;

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

    impl<OPERAND, Atom> Prioritized for AnyConnective<OPERAND, Atom> {
        fn priority(&self) -> Priority {
            match self {
                Self::Nullary(op) => op.connective.priority(),
                Self::Unary(op) => op.connective.priority(),
                Self::Binary(op) => op.connective.priority(),
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
        let x = DynConnective::<2, _, ()>::new(Conjunction, [(), ()]);
        let y = DynConnective::new(Disjunction, [(), ()]);
        let z = DynConnective::new(Conjunction, [(), ()]);

        assert_ne!(x, y);
        assert_eq!(x, z);
    }

    #[test]
    fn any_equality_with_clearing_operands() {
        let x = AnyConnective::<_, ()>::Binary(DynConnective::new(Conjunction, [(), ()]));
        let y = AnyConnective::<_, ()>::Binary(DynConnective::new(Disjunction, [(), ()]));
        let z = AnyConnective::<_, ()>::Binary(DynConnective::new(Conjunction, [(), ()]));

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
        let x = AnyConnective::<_, ()>::Binary(DynConnective::new(
            Conjunction,
            ["a".to_string(), "b".to_string()],
        ));
        let y = AnyConnective::<_, ()>::Binary(DynConnective::new(
            Disjunction,
            ["a".to_string(), "b".to_string()],
        ));
        let z = AnyConnective::<_, ()>::Binary(DynConnective::new(
            Conjunction,
            ["a".to_string(), "b".to_string()],
        ));

        assert_ne!(x, y);
        assert_eq!(x, z);

        let x_ref = x.as_ref::<str>();
        let y_ref = y.as_ref();
        let z_ref = z.as_ref();

        assert_ne!(x_ref, y_ref);
        assert_eq!(x_ref, z_ref);
    }
}
