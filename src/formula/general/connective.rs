use std::{borrow::Borrow, fmt::Debug};

use derive_where::derive_where;

use crate::{
    connective::{Connective, Prioritized, TruthFn},
    utils::{upcast::Upcast as _, Zst},
};

use super::formula::Formula;

use self::usable::UsableConnective;

#[derive_where(Debug; OPERAND: Debug)]
#[derive_where(Clone; OPERAND: Clone)]
#[derive_where(PartialEq; OPERAND: PartialEq, VAR: 'static)]
#[derive_where(Eq; OPERAND: Eq, VAR: 'static)]
/// [`Connective`] + [`TruthFn`] of ARITY from {0, 1, 2} along with their operands.
pub enum AnyConnective<OPERAND, VAR> {
    /// Nullary [`Connective`].
    Nullary(DynConnective<0, OPERAND, VAR>),
    /// Unary [`Connective`].
    Unary(DynConnective<1, OPERAND, VAR>),
    /// Binary [`Connective`].
    Binary(DynConnective<2, OPERAND, VAR>),
}

impl<OPERAND, VAR> AnyConnective<OPERAND, VAR> {
    /// Create a [`DynConnective`] with a [`Connective<0>`].
    pub fn nullary<C>(connective: C) -> Self
    where
        C: Connective<0>
            + TruthFn<0, Formula<VAR>>
            + Prioritized
            + Debug
            + Clone
            + PartialEq
            + 'static,
    {
        Self::Nullary(DynConnective::new(connective, []))
    }

    /// Create a [`DynConnective`] with a [`Connective<1>`].
    pub fn unary<C>(connective: C, operand: OPERAND) -> Self
    where
        C: Connective<1>
            + TruthFn<1, Formula<VAR>>
            + Prioritized
            + Debug
            + Clone
            + PartialEq
            + 'static,
    {
        Self::Unary(DynConnective::new(connective, [operand]))
    }

    /// Create a [`DynConnective`] with a [`Connective<2>`].
    pub fn binary<C>(connective: C, operands: (OPERAND, OPERAND)) -> Self
    where
        C: Connective<2>
            + TruthFn<2, Formula<VAR>>
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
    pub fn clear_operands(&self) -> AnyConnective<(), VAR> {
        self.get_borrowed::<OPERAND>().map(drop)
    }

    /// The 'reference' version of [`AnyConnective`].
    pub fn get_borrowed<U: ?Sized>(&self) -> AnyConnective<&U, VAR>
    where
        OPERAND: Borrow<U>,
    {
        match self {
            Self::Nullary(x) => AnyConnective::Nullary(x.get_borrowed()),
            Self::Unary(x) => AnyConnective::Unary(x.get_borrowed()),
            Self::Binary(x) => AnyConnective::Binary(x.get_borrowed()),
        }
    }

    /// Convert to another [`AnyConnective`] by converting its operands.
    pub fn map<F, OperandTarget>(self, f: F) -> AnyConnective<OperandTarget, VAR>
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

impl<VAR> AnyConnective<Box<Formula<VAR>>, VAR> {
    /// Convert [`AnyConnective`] into the [`Formula`].
    ///
    /// If the `is_dynamic` is `true`, keep the dynamic nature of the connective
    /// by returning [`Formula::Dynamic`].
    /// Otherwise, convert it into the [canonical form][Self::into_canonical].
    pub fn into_formula(self, is_dynamic: bool) -> Formula<VAR> {
        if is_dynamic {
            Formula::Dynamic(self)
        } else {
            self.into_canonical()
        }
    }

    /// Convert [`AnyConnective`] into the [`Formula`]s typed variants.
    pub fn into_canonical(self) -> Formula<VAR> {
        use crate::{
            connective::{functions, TruthFn},
            truth_table::TruthTabled as _,
        };

        match self {
            Self::Nullary(conn) => {
                conn.map(|f| *f)
                    .compose_if_equivalent(functions::Falsity)
                    .or_else(|conn| conn.compose_if_equivalent(functions::Truth))
                    // should be unreachable because of exhaustive checks before
                    .unwrap_or_else(DynConnective::compose)
            }

            Self::Unary(conn) => {
                conn.map(|f| *f)
                    .compose_if_equivalent(functions::Falsity)
                    .or_else(|conn| conn.compose_if_equivalent(functions::LogicalIdentity))
                    .or_else(|conn| conn.compose_if_equivalent(functions::Negation))
                    .or_else(|conn| conn.compose_if_equivalent(functions::Truth))
                    // should be unreachable because of exhaustive checks before
                    .unwrap_or_else(DynConnective::compose)
            }
            Self::Binary(conn) => {
                conn.map(|f| *f)
                    .compose_if_equivalent(functions::Falsity)
                    .or_else(|conn| conn.compose_if_equivalent(functions::Conjunction))
                    .or_else(|conn| conn.compose_if_equivalent(functions::MaterialNonImplication))
                    .or_else(|conn| conn.compose_if_equivalent(functions::First {}))
                    .or_else(|conn| conn.compose_if_equivalent(functions::ConverseNonImplication))
                    .or_else(|conn| conn.compose_if_equivalent(functions::Last {}))
                    .or_else(|conn| conn.compose_if_equivalent(functions::ExclusiveDisjunction))
                    .or_else(|conn| conn.compose_if_equivalent(functions::Disjunction))
                    .or_else(|conn| conn.compose_if_equivalent(functions::NonDisjunction))
                    .or_else(|conn| conn.compose_if_equivalent(functions::LogicalBiconditional))
                    // .or_else(|conn| conn.compose_if_equivalent(functions::NotSecond::new()))
                    .or_else(|conn| conn.compose_if_equivalent(functions::ConverseImplication))
                    // .or_else(|conn| conn.compose_if_equivalent(functions::NotFirst::new()))
                    .or_else(|conn| conn.compose_if_equivalent(functions::MaterialImplication))
                    .or_else(|conn| conn.compose_if_equivalent(functions::NonConjunction))
                    .or_else(|conn| conn.compose_if_equivalent(functions::Truth))
                    .unwrap_or_else(
                        |DynConnective {
                             connective,
                             operands: [f1, f2],
                         }| {
                            use super::super::ops::Not as _;

                            if connective.is_equivalent(&functions::NotSecond::new()) {
                                Formula::not(f2)
                            } else if connective.is_equivalent(&functions::NotFirst::new()) {
                                Formula::not(f1)
                            } else {
                                // should be unreachable because of exhaustive checks before
                                connective.compose([f1, f2])
                            }
                        },
                    )
            }
        }
    }
}

impl<VAR> AnyConnective<Formula<VAR>, VAR> {
    /// Convert [`AnyConnective`] into the [`Formula`].
    ///
    /// If the `is_dynamic` is `true`, keep the dynamic nature of the connective
    /// by returning [`Formula::Dynamic`].
    /// Otherwise, convert it into the [canonical form][Self::into_canonical].
    pub fn into_formula(self, is_dynamic: bool) -> Formula<VAR> {
        self.map(Box::new).into_formula(is_dynamic)
    }

    /// Convert [`AnyConnective`] into the [`Formula`]s typed variants.
    pub fn into_canonical(self) -> Formula<VAR> {
        self.map(Box::new).into_canonical()
    }
}

impl<VAR> From<AnyConnective<Box<Self>, VAR>> for Formula<VAR> {
    fn from(value: AnyConnective<Box<Self>, VAR>) -> Self {
        value.into_formula(true)
    }
}

impl<VAR> From<AnyConnective<Self, VAR>> for Formula<VAR> {
    fn from(value: AnyConnective<Self, VAR>) -> Self {
        value.into_formula(true)
    }
}

#[derive_where(Debug; OPERAND: Debug)]
#[derive_where(Clone; OPERAND: Clone)]
// requires `VAR: 'static` because of the `UsableConnective`: `DynCompare`: `AsDynCompare`: `Any`: `'static`
#[derive_where(PartialEq; OPERAND: PartialEq, VAR: 'static)]
#[derive_where(Eq; OPERAND: Eq, VAR: 'static)]
/// Wrapper for dynamic [`Connective`] and [`TruthFn`] with operands attached.
pub struct DynConnective<const ARITY: usize, OPERAND, VAR> {
    pub(super) connective: Box<dyn UsableConnective<ARITY, VAR>>,
    pub(super) operands: [OPERAND; ARITY],
}

impl<const ARITY: usize, OPERAND, VAR> DynConnective<ARITY, OPERAND, VAR> {
    /// Create a [`DynConnective`] with a [`Connective<0>`].
    pub fn new<C>(connective: C, operands: [OPERAND; ARITY]) -> Self
    where
        C: Connective<ARITY>
            + TruthFn<ARITY, Formula<VAR>>
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
    pub fn clear_operands(&self) -> DynConnective<ARITY, (), VAR> {
        DynConnective {
            connective: self.connective.clone(),
            operands: [(); ARITY],
        }
    }

    /// The 'reference' version of [`DynConnective`].
    pub fn get_borrowed<U: ?Sized>(&self) -> DynConnective<ARITY, &U, VAR>
    where
        OPERAND: Borrow<U>,
    {
        let operands = self.operands.each_ref().map(OPERAND::borrow);
        DynConnective {
            connective: self.connective.clone(),
            operands,
        }
    }

    /// Convert to another [`DynConnective`] by converting its operands.
    pub fn map<F, OperandTarget>(self, f: F) -> DynConnective<ARITY, OperandTarget, VAR>
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

    fn compose_if_equivalent<F>(self, truth_fn: F) -> Result<OPERAND, Self>
    where
        OPERAND: crate::connective::Evaluable,
        crate::arity::two_powers::D: crate::CheckedArray<ARITY>,
        F: crate::connective::BoolFn<ARITY> + TruthFn<ARITY, OPERAND>,
    {
        use crate::truth_table::TruthTabled as _;

        if self.connective.is_equivalent(&truth_fn) {
            Ok(truth_fn.compose(self.operands))
        } else {
            Err(self)
        }
    }
}

impl<const ARITY: usize, VAR> DynConnective<ARITY, Formula<VAR>, VAR> {
    fn compose(self) -> Formula<VAR> {
        let Self {
            connective,
            operands,
        } = self;
        connective.compose(operands)
    }
}

impl<'a, const ARITY: usize, OPERAND, VAR: 'a> AsRef<dyn Connective<ARITY> + 'a>
    for DynConnective<ARITY, OPERAND, VAR>
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
    pub(in super::super) trait UsableConnective<const N: usize, VAR>:
        Connective<N> + TruthFn<N, Formula<VAR>> + Prioritized + Debug + DynClone + DynCompare
    {
    }

    impl<const N: usize, VAR, T> UsableConnective<N, VAR> for T where
        T: Connective<N> + TruthFn<N, Formula<VAR>> + Prioritized + Debug + DynClone + DynCompare
    {
    }

    clone_trait_object!(<const N: usize, VAR> UsableConnective<N, VAR>);

    // Need `VAR: 'static` to ensure conversion to `AsDynCompare`
    // and the `AsDynCompare` requires `Any`, that in turn requires `'static`.
    impl<const N: usize, VAR: 'static> PartialEq for Box<dyn UsableConnective<N, VAR> + '_> {
        fn eq(&self, other: &Self) -> bool {
            self.as_dyn_compare() == other.as_dyn_compare()
        }
    }

    impl<const N: usize, VAR: 'static> Eq for Box<dyn UsableConnective<N, VAR> + '_> {}
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

    impl<OPERAND, VAR> Prioritized for AnyConnective<OPERAND, VAR> {
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
