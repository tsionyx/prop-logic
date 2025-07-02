use std::{borrow::Borrow, fmt::Debug};

use derive_where::derive_where;

use crate::{
    connective::{Connective, Prioritized, TruthFn},
    utils::{upcast::Upcast as _, Zst},
};

use super::formula::Formula;

use self::usable::UsableConnective;

#[derive_where(Debug; Operand: Debug)]
#[derive_where(Clone; Operand: Clone)]
#[derive_where(PartialEq; Operand: PartialEq, Var: 'static)]
#[derive_where(Eq; Operand: Eq, Var: 'static)]
/// [`Connective`] + [`TruthFn`] of ARITY from {0, 1, 2} along with their operands.
pub enum AnyConnective<Operand, Var> {
    /// Nullary [`Connective`].
    Nullary(DynConnective<0, Operand, Var>),
    /// Unary [`Connective`].
    Unary(DynConnective<1, Operand, Var>),
    /// Binary [`Connective`].
    Binary(DynConnective<2, Operand, Var>),
}

impl<Operand, Var> AnyConnective<Operand, Var> {
    /// Create a [`DynConnective`] with a [`Connective<0>`].
    pub fn nullary<C>(connective: C) -> Self
    where
        C: Connective<0>
            + TruthFn<0, Formula<Var>>
            + Prioritized
            + Debug
            + Clone
            + PartialEq
            + 'static,
    {
        Self::Nullary(DynConnective::new(connective, []))
    }

    /// Create a [`DynConnective`] with a [`Connective<1>`].
    pub fn unary<C>(connective: C, operand: Operand) -> Self
    where
        C: Connective<1>
            + TruthFn<1, Formula<Var>>
            + Prioritized
            + Debug
            + Clone
            + PartialEq
            + 'static,
    {
        Self::Unary(DynConnective::new(connective, [operand]))
    }

    /// Create a [`DynConnective`] with a [`Connective<2>`].
    pub fn binary<C>(connective: C, operands: (Operand, Operand)) -> Self
    where
        C: Connective<2>
            + TruthFn<2, Formula<Var>>
            + Prioritized
            + Debug
            + Clone
            + PartialEq
            + 'static,
    {
        Self::Binary(DynConnective::new(connective, operands.into()))
    }

    /// Transform the connective by reversing order of operands
    /// if it is a [binary][Self::Binary] function.
    pub fn swap_operands(self) -> Self {
        if let Self::Binary(DynConnective {
            connective,
            operands: [x1, x2],
        }) = self
        {
            Self::Binary(DynConnective {
                connective,
                operands: [x2, x1],
            })
        } else {
            self
        }
    }

    /// Forget the operands and return 'only-operator' version of [`AnyConnective`].
    pub fn clear_operands(&self) -> AnyConnective<(), Var> {
        self.get_borrowed::<Operand>().map(drop)
    }

    /// The 'reference' version of [`AnyConnective`].
    pub fn get_borrowed<U: ?Sized>(&self) -> AnyConnective<&U, Var>
    where
        Operand: Borrow<U>,
    {
        match self {
            Self::Nullary(x) => AnyConnective::Nullary(x.get_borrowed()),
            Self::Unary(x) => AnyConnective::Unary(x.get_borrowed()),
            Self::Binary(x) => AnyConnective::Binary(x.get_borrowed()),
        }
    }

    /// Convert to another [`AnyConnective`] by converting its operands.
    pub fn map<F, OperandDst>(self, f: F) -> AnyConnective<OperandDst, Var>
    where
        F: FnMut(Operand) -> OperandDst,
    {
        match self {
            Self::Nullary(x) => AnyConnective::Nullary(x.map(f)),
            Self::Unary(x) => AnyConnective::Unary(x.map(f)),
            Self::Binary(x) => AnyConnective::Binary(x.map(f)),
        }
    }
}

impl<Var> AnyConnective<Box<Formula<Var>>, Var> {
    /// Convert [`AnyConnective`] into the [`Formula`].
    ///
    /// If the `is_dynamic` is `true`, keep the dynamic nature of the connective
    /// by returning [`Formula::Dynamic`].
    /// Otherwise, convert it into the [canonical form][Self::into_canonical].
    pub fn into_formula(self, is_dynamic: bool) -> Formula<Var> {
        if is_dynamic {
            Formula::Dynamic(self)
        } else {
            self.into_canonical()
        }
    }

    /// Convert [`AnyConnective`] into the [`Formula`]s typed variants.
    pub fn into_canonical(self) -> Formula<Var> {
        match self.map(|f| *f) {
            AnyConnective::Nullary(conn) => conn.compose(),
            AnyConnective::Unary(conn) => conn.compose(),
            AnyConnective::Binary(conn) => conn.compose(),
        }
    }
}

impl<Var> AnyConnective<Formula<Var>, Var> {
    /// Convert [`AnyConnective`] into the [`Formula`].
    ///
    /// If the `is_dynamic` is `true`, keep the dynamic nature of the connective
    /// by returning [`Formula::Dynamic`].
    /// Otherwise, convert it into the [canonical form][Self::into_canonical].
    pub fn into_formula(self, is_dynamic: bool) -> Formula<Var> {
        self.map(Box::new).into_formula(is_dynamic)
    }

    /// Convert [`AnyConnective`] into the [`Formula`]s typed variants.
    pub fn into_canonical(self) -> Formula<Var> {
        self.map(Box::new).into_canonical()
    }
}

impl<Var> From<AnyConnective<Box<Self>, Var>> for Formula<Var> {
    fn from(value: AnyConnective<Box<Self>, Var>) -> Self {
        value.into_formula(true)
    }
}

impl<Var> From<AnyConnective<Self, Var>> for Formula<Var> {
    fn from(value: AnyConnective<Self, Var>) -> Self {
        value.into_formula(true)
    }
}

#[derive_where(Debug; Operand: Debug)]
#[derive_where(Clone; Operand: Clone)]
// requires `Var: 'static` because of the `UsableConnective`: `DynCompare`: `AsDynCompare`: `Any`: `'static`
#[derive_where(PartialEq; Operand: PartialEq, Var: 'static)]
#[derive_where(Eq; Operand: Eq, Var: 'static)]
/// Wrapper for dynamic [`Connective`] and [`TruthFn`] with operands attached.
pub struct DynConnective<const ARITY: usize, Operand, Var> {
    pub(super) connective: Box<dyn UsableConnective<ARITY, Var>>,
    pub(super) operands: [Operand; ARITY],
}

impl<const ARITY: usize, Operand, Var> DynConnective<ARITY, Operand, Var> {
    /// Create a [`DynConnective`] with a [`Connective<0>`].
    pub fn new<C>(connective: C, operands: [Operand; ARITY]) -> Self
    where
        C: Connective<ARITY>
            + TruthFn<ARITY, Formula<Var>>
            + Prioritized
            + Debug
            + Clone
            + PartialEq
            + 'static,
    {
        #[expect(path_statements)]
        {
            C::ASSERT_ZST; // `Sized` ensured by `Clone`
        }

        Self {
            connective: Box::new(connective),
            operands,
        }
    }

    /// Forget the operands and return 'only-operator' version of [`DynConnective`].
    pub fn clear_operands(&self) -> DynConnective<ARITY, (), Var> {
        DynConnective {
            connective: self.connective.clone(),
            operands: [(); ARITY],
        }
    }

    /// The 'reference' version of [`DynConnective`].
    pub fn get_borrowed<U: ?Sized>(&self) -> DynConnective<ARITY, &U, Var>
    where
        Operand: Borrow<U>,
    {
        let operands = self.operands.each_ref().map(Operand::borrow);
        DynConnective {
            connective: self.connective.clone(),
            operands,
        }
    }

    /// Convert to another [`DynConnective`] by converting its operands.
    pub fn map<F, OperandDst>(self, f: F) -> DynConnective<ARITY, OperandDst, Var>
    where
        F: FnMut(Operand) -> OperandDst,
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

impl<const ARITY: usize, Var> DynConnective<ARITY, Formula<Var>, Var> {
    fn compose(self) -> Formula<Var> {
        let Self {
            connective,
            operands,
        } = self;
        connective.compose(operands)
    }
}

impl<'a, const ARITY: usize, Operand, Var: 'a> DynConnective<ARITY, Operand, Var> {
    /// Get the reference to the inner `Connective`.
    pub fn get_connective(&self) -> &(dyn Connective<ARITY> + 'a) {
        self.connective.up()
    }
}

mod usable {
    use std::fmt::Debug;

    use dyn_clone::{clone_trait_object, DynClone};

    use crate::utils::dyn_eq::DynCompare;

    use super::{Connective, Formula, Prioritized, TruthFn};

    /// [`Connective`]'s marker subtrait to be used in [`DynConnective`][super::DynConnective].
    pub(in super::super) trait UsableConnective<const N: usize, Var>:
        Connective<N> + TruthFn<N, Formula<Var>> + Prioritized + Debug + DynClone + DynCompare
    {
    }

    impl<const N: usize, Var, T> UsableConnective<N, Var> for T where
        T: Connective<N> + TruthFn<N, Formula<Var>> + Prioritized + Debug + DynClone + DynCompare
    {
    }

    clone_trait_object!(<const N: usize, Var> UsableConnective<N, Var>);

    // Need `Var: 'static` to ensure conversion to `AsDynCompare`
    // and the `AsDynCompare` requires `Any`, that in turn requires `'static`.
    impl<const N: usize, Var: 'static> PartialEq for Box<dyn UsableConnective<N, Var> + '_> {
        fn eq(&self, other: &Self) -> bool {
            self.as_dyn_compare() == other.as_dyn_compare()
        }
    }

    impl<const N: usize, Var: 'static> Eq for Box<dyn UsableConnective<N, Var> + '_> {}
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

    // most common operations' priorities
    impl_priority!(Falsity, LogicalIdentity, Truth: 255);
    impl_priority!(Negation: 200);
    impl_priority!(Conjunction: 100);
    impl_priority!(Disjunction, ExclusiveDisjunction: 100);
    impl_priority!(LogicalBiconditional, MaterialImplication, MaterialNonImplication, ConverseImplication, ConverseNonImplication: 90);
    impl_priority!(NonConjunction, NonDisjunction: 80);
    impl_priority!(First, Last, NotFirst, NotSecond: 10);

    impl<Operand, Var> Prioritized for AnyConnective<Operand, Var> {
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

        let x_ref = x.get_borrowed::<str>();
        let y_ref = y.get_borrowed();
        let z_ref = z.get_borrowed();

        assert_ne!(x_ref, y_ref);
        assert_eq!(x_ref, z_ref);
    }
}

#[cfg(all(test, feature = "arbitrary"))]
mod prop_test {
    use proptest::prelude::*;

    use super::super::super::{Formula, FormulaParameters};

    fn params_static() -> FormulaParameters<char> {
        FormulaParameters {
            variables: vec!['a', 'b', 'c', 'd'],
            leaf_var_weight: Some(10),
            ..FormulaParameters::default()
        }
    }

    fn params_dynamic() -> FormulaParameters<char> {
        FormulaParameters {
            use_dynamic: true,
            ..params_static()
        }
    }

    proptest! {
        // https://proptest-rs.github.io/proptest/proptest/tutorial/config.html
        #![proptest_config(ProptestConfig::with_cases(1000))]

        #[test]
        fn round_trip_static_any_connective(f in Formula::arbitrary_with(params_static())) {
            let f2 = f.get_connective().map(Clone::clone);
            println!("{f} -> {f2:?}");
            assert_eq!(f, f2.clone().into_canonical());
            assert_eq!(f, f2.into_formula(false));
        }

        #[test]
        fn round_trip_dymanic_any_connective(f in Formula::arbitrary_with(params_dynamic())) {
            let f2 = f.get_connective().map(Clone::clone);
            println!("{f} -> {f2:?}");
            assert_eq!(f, f2.into_formula(f.is_dynamic()));
        }
    }
}
