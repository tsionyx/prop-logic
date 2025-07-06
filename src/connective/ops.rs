//! Unary operations and properties of the [`BoolFn`]-s.
use std::{
    collections::HashMap as Map,
    fmt::{self, Display},
    ops::Not,
};

use crate::{truth_table::TruthTabled as _, utils::vec::UnsortedVec};

#[allow(clippy::wildcard_imports)]
use super::{
    evaluation::Evaluable, functions::*, ternary::Ternary, BoolFn, Connective, InitFn as _, TruthFn,
};

/// Easily convert a `BoolFn` into its counterpart in terms
/// of switching all the bits in its truth table.
#[auto_impl::auto_impl(&, Box)]
pub trait Negate<const ARITY: usize>: BoolFn<ARITY> {
    /// Another `BoolFn` which truth table is a negation of the original one.
    type Not: BoolFn<ARITY>;
}

macro_rules! impl_negate {
    ($arity:literal: $($t:ty =! $neg_t:ty),+ $(,)?) => {
        $(
            impl Negate<$arity> for $t
            {
                type Not = $neg_t;
            }

            impl Negate<$arity> for $neg_t
            {
                type Not = $t;
            }
        )+
    };
}

macro_rules! impl_std_not {
    ($arity:literal: $($t:ty),+ $(,)?) => {
        $(
            impl Not for $t
            {
                type Output = <$t as Negate<$arity>>::Not;

                fn not(self) -> Self::Output {
                    <$t as Negate<$arity>>::Not::init()
                }
            }
        )+
    };
}

/// Easily convert a binary `BoolFn` into its counterpart in terms
/// of swapping its arguments.
///
/// For the _commutative_ operation, the [`Converse::Conversion`]
/// is equivalent to the function itself.
#[auto_impl::auto_impl(&, Box)]
pub trait Converse: BoolFn<2> {
    /// Another `BoolFn` which truth function is an conversion of the original one.
    type Conversion: BoolFn<2>;
}

macro_rules! impl_converse {
    ($($t:ty =! $con_t:ty),+ $(,)?) => {
        $(
            impl Converse for $t
            {
                type Conversion = $con_t;
            }

            impl Converse for $con_t
            {
                type Conversion = $t;
            }
        )+
    };

    // commutative case
    ($($t:ty),+ $(,)?) => {
        $(
            impl Converse for $t
            {
                type Conversion = Self;
            }
        )+
    };
}

// TODO: impl std::ops::Neg? (could be confusing)

// implementations for nullary and unary
impl<const ARITY: usize> Negate<ARITY> for Falsity {
    type Not = Truth;
}

impl Not for Falsity {
    type Output = Truth;

    fn not(self) -> Self::Output {
        Truth
    }
}

impl_negate![1: LogicalIdentity =! Negation];
impl_std_not![1: LogicalIdentity, Negation];

impl<const ARITY: usize> Negate<ARITY> for Truth {
    type Not = Falsity;
}

impl Not for Truth {
    type Output = Falsity;

    fn not(self) -> Self::Output {
        Falsity
    }
}

// implementations for binary
impl_negate![2:
    Conjunction =! NonConjunction,
    MaterialNonImplication =! MaterialImplication,
    First =! NotFirst,
    ConverseNonImplication =! ConverseImplication,
    Last =! NotSecond,
    ExclusiveDisjunction =! LogicalBiconditional,
    Disjunction =! NonDisjunction,
];
impl_std_not![2:
    Conjunction, NonConjunction,
    MaterialNonImplication, MaterialImplication,
    First, NotFirst,
    ConverseNonImplication, ConverseImplication,
    Last, NotSecond,
    ExclusiveDisjunction, LogicalBiconditional,
    Disjunction, NonDisjunction,
];

impl_converse![
    Falsity,
    Conjunction,
    ExclusiveDisjunction,
    Disjunction,
    NonDisjunction,
    LogicalBiconditional,
    NonConjunction,
    Truth
];
impl_converse![
    MaterialNonImplication = !ConverseNonImplication,
    First = !Last,
    MaterialImplication = !ConverseImplication,
    NotFirst = !NotSecond,
];

/// Allow to check the
/// [commutativity property](https://en.wikipedia.org/wiki/Commutative_property#Truth_functional_connectives)
/// of a binary [`BoolFn`].
pub trait Commutativity {
    /// Whether the given [`BoolFn`] returns the same result
    /// regardles of its arguments' order.
    fn is_commutative(&self) -> bool;
}

impl<F> Commutativity for F
where
    F: BoolFn<2>,
{
    fn is_commutative(&self) -> bool {
        use crate::truth_table::TruthTabled as _;

        let table: Map<_, _> = self.get_truth_table().into_iter().collect();

        table.iter().all(|(args, val)| {
            let conversed = [args[1], args[0]];
            table.get(&conversed) == Some(val)
        })
    }
}

/// Allow to check the
/// [associativity property](https://en.wikipedia.org/wiki/Associative_property#Truth_functional_connectives)
/// of a binary [`BoolFn`].
pub trait Associativity {
    /// Whether the given binary [`BoolFn`]
    /// can be evaluated in arbitrary order while chained.
    fn is_associative(&self) -> bool;
}

impl<F> Associativity for F
where
    F: BoolFn<2>,
{
    fn is_associative(&self) -> bool {
        let t_left = Ternary::<true, _>::new(self, self);
        let t_right = Ternary::<false, _>::new(self, self);
        // println!("{}", t_left.get_truth_table());
        // println!("{}", t_right.get_truth_table());
        t_left.is_equivalent(&t_right)
    }
}

/// Allow to introduce the concept of
/// a (two-sided) neutral element for a binary function.
pub trait Neutrality<E: Evaluable>: TruthFn<2, E> {
    /// An [identity element](https://en.wikipedia.org/wiki/Identity_element)
    /// of a binary function.
    fn identity_element(&self) -> E;
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Combination of items connected with
/// the binary operation.
pub struct Series<T, Op> {
    op: Op,
    repr: UnsortedVec<T>,
}

impl<T, Op> Series<T, Op> {
    /// Construct a new [`Series`] from a bunch of items.
    pub fn new(items: impl IntoIterator<Item = T>) -> Self
    where
        Op: Default,
    {
        Self {
            op: Op::default(),
            repr: items.into_iter().collect(),
        }
    }

    /// Get the glue operation for a series.
    pub const fn operation(&self) -> &Op {
        &self.op
    }

    /// Create an [`Evaluable`] from a bunch of items.
    pub fn compose<E>(self) -> E
    where
        E: Evaluable,
        T: Into<E>,
        Op: Neutrality<E>,
    {
        series(&self.op, self.repr.into_iter().map(T::into))
    }

    pub(crate) const fn as_unsorted(&self) -> &UnsortedVec<T> {
        &self.repr
    }
}

impl<T, Op> AsRef<[T]> for Series<T, Op> {
    fn as_ref(&self) -> &[T] {
        self.repr.as_ref()
    }
}

impl<T, Op> IntoIterator for Series<T, Op> {
    type Item = T;

    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.repr.into_iter()
    }
}

impl<T: Display, Op: Connective<2>> Display for Series<T, Op> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let notation = self.op.notation();

        match self.as_ref() {
            [] => {
                Display::fmt(&notation, f)?;
                write!(f, "∅")
            }
            [single] => Display::fmt(single, f),
            many => {
                Display::fmt(&notation, f)?;
                write!(f, "(")?;

                let mut first = true;
                for item in many {
                    if !first {
                        write!(f, ", ")?;
                    }
                    Display::fmt(item, f)?;
                    first = false;
                }

                write!(f, ")")
            }
        }
    }
}

/// Combine an arbitrary series of [`Evaluable`]
/// items using an operation [with identity element][Neutrality::identity_element]
pub fn series<I, F, E>(f: &F, it: I) -> E
where
    I: DoubleEndedIterator<Item = E>,
    F: Neutrality<E>,
    E: Evaluable,
{
    it.rfold(None, |acc, ev| {
        let ev = if let Some(acc) = acc {
            f.compose([ev, acc])
        } else {
            ev
        };
        Some(ev)
    })
    .unwrap_or_else(|| f.identity_element())
}

impl<E> Neutrality<E> for Conjunction
where
    E: Evaluable + crate::formula::And,
{
    fn identity_element(&self) -> E {
        E::tautology()
    }
}

impl<E> Neutrality<E> for Disjunction
where
    E: Evaluable + crate::formula::Or,
{
    fn identity_element(&self) -> E {
        E::contradiction()
    }
}

impl<E> Neutrality<E> for ExclusiveDisjunction
where
    E: Evaluable + crate::formula::Xor + crate::formula::Not,
{
    fn identity_element(&self) -> E {
        E::contradiction()
    }
}

impl<E> Neutrality<E> for LogicalBiconditional
where
    E: Evaluable + crate::formula::Equivalent + crate::formula::Not,
{
    fn identity_element(&self) -> E {
        E::tautology()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        arity::two_powers,
        truth_table::{TruthTable as _, TruthTabled as _},
        utils::dependent_array::CheckedArray,
    };

    use super::{super::InitFn, *};

    fn assert_neg<const ARITY: usize, N>()
    where
        N: Negate<ARITY> + BoolFn<ARITY> + InitFn,
        N::Not: BoolFn<ARITY> + InitFn,
        two_powers::D: CheckedArray<ARITY>,
    {
        let table = N::init().get_truth_table().values();
        let table_neg = N::Not::init().get_truth_table().values();

        dbg!(std::any::type_name::<N>());
        dbg!(std::any::type_name::<N::Not>());
        for (x, y) in table.into_iter().zip(table_neg) {
            assert_eq!(x, !y);
        }
    }

    fn assert_std_not<const ARITY: usize, N, N2>(x: N)
    where
        N: Not<Output = N2> + BoolFn<ARITY>,
        N2: BoolFn<ARITY>,
        two_powers::D: CheckedArray<ARITY>,
    {
        let table = x.get_truth_table().values();
        let table_neg = (!x).get_truth_table().values();

        dbg!(std::any::type_name::<N>());
        for (x, y) in table.into_iter().zip(table_neg) {
            assert_eq!(x, !y);
        }
    }

    fn assert_conversion<C>()
    where
        C: Converse + BoolFn<2> + InitFn,
        C::Conversion: BoolFn<2> + InitFn,
    {
        let table = C::init().get_truth_table().into_iter();
        let table_conversed: HashMap<_, _> = C::Conversion::init()
            .get_truth_table()
            .into_iter()
            .collect();
        assert_eq!(table_conversed.len(), 4);

        dbg!(std::any::type_name::<C>());
        dbg!(std::any::type_name::<C::Conversion>());
        for ([x, y], val) in table {
            let conversed_args = [y, x];
            let conversed_val = *table_conversed
                .get(&conversed_args)
                .expect("Should contain all combinations");
            assert_eq!(val, conversed_val);
        }
    }

    #[test]
    fn test_all_negations() {
        // nullary
        assert_neg::<0, Falsity>();
        assert_std_not::<0, _, _>(Falsity);

        assert_neg::<0, Truth>();
        assert_std_not::<0, _, _>(Truth);

        // unary
        assert_neg::<1, Falsity>();
        assert_std_not::<1, _, _>(Falsity);

        assert_neg::<1, LogicalIdentity>();
        assert_std_not::<1, _, _>(LogicalIdentity);

        assert_neg::<1, Negation>();
        assert_std_not::<1, _, _>(Negation);

        assert_neg::<1, Truth>();
        assert_std_not::<1, _, _>(Truth);

        // binary
        assert_neg::<2, Falsity>();
        assert_std_not::<2, _, _>(Falsity);

        assert_neg::<2, Conjunction>();
        assert_std_not::<2, _, _>(Conjunction);

        assert_neg::<2, MaterialNonImplication>();
        assert_std_not::<2, _, _>(MaterialNonImplication);

        assert_neg::<2, First>();
        assert_std_not::<2, _, _>(First {});

        assert_neg::<2, ConverseNonImplication>();
        assert_std_not::<2, _, _>(ConverseNonImplication);

        assert_neg::<2, Last>();
        assert_std_not::<2, _, _>(Last {});

        assert_neg::<2, ExclusiveDisjunction>();
        assert_std_not::<2, _, _>(ExclusiveDisjunction);

        assert_neg::<2, Disjunction>();
        assert_std_not::<2, _, _>(Disjunction);

        assert_neg::<2, NonDisjunction>();
        assert_std_not::<2, _, _>(NonDisjunction);

        assert_neg::<2, LogicalBiconditional>();
        assert_std_not::<2, _, _>(LogicalBiconditional);

        assert_neg::<2, NotSecond>();
        assert_std_not::<2, _, _>(NotSecond::new());

        assert_neg::<2, ConverseImplication>();
        assert_std_not::<2, _, _>(ConverseImplication);

        assert_neg::<2, NotFirst>();
        assert_std_not::<2, _, _>(NotFirst::new());

        assert_neg::<2, MaterialImplication>();
        assert_std_not::<2, _, _>(MaterialImplication);

        assert_neg::<2, NonConjunction>();
        assert_std_not::<2, _, _>(NonConjunction);

        assert_neg::<2, Truth>();
        assert_std_not::<2, _, _>(Truth);
    }

    #[test]
    fn test_all_conversions() {
        assert_conversion::<Falsity>();
        assert_conversion::<Conjunction>();
        assert_conversion::<MaterialNonImplication>();
        assert_conversion::<First>();
        assert_conversion::<ConverseNonImplication>();
        assert_conversion::<Last>();
        assert_conversion::<ExclusiveDisjunction>();
        assert_conversion::<Disjunction>();
        assert_conversion::<NonDisjunction>();
        assert_conversion::<LogicalBiconditional>();
        assert_conversion::<NotSecond>();
        assert_conversion::<ConverseImplication>();
        assert_conversion::<NotFirst>();
        assert_conversion::<MaterialImplication>();
        assert_conversion::<NonConjunction>();
        assert_conversion::<Truth>();
    }

    fn assert_commutativity<F>(holds: bool)
    where
        F: BoolFn<2> + InitFn,
    {
        assert_eq!(F::init().is_commutative(), holds);
    }

    #[test]
    fn test_all_commutativity() {
        assert_commutativity::<Falsity>(true);
        assert_commutativity::<Conjunction>(true);
        assert_commutativity::<MaterialNonImplication>(false);
        assert_commutativity::<First>(false);
        assert_commutativity::<ConverseNonImplication>(false);
        assert_commutativity::<Last>(false);
        assert_commutativity::<ExclusiveDisjunction>(true);
        assert_commutativity::<Disjunction>(true);
        assert_commutativity::<NonDisjunction>(true);
        assert_commutativity::<LogicalBiconditional>(true);
        assert_commutativity::<NotSecond>(false);
        assert_commutativity::<ConverseImplication>(false);
        assert_commutativity::<NotFirst>(false);
        assert_commutativity::<MaterialImplication>(false);
        assert_commutativity::<NonConjunction>(true);
        assert_commutativity::<Truth>(true);
    }

    fn assert_associativity<F>(holds: bool)
    where
        F: BoolFn<2> + InitFn,
    {
        assert_eq!(F::init().is_associative(), holds);
    }

    #[test]
    fn test_all_associativity() {
        assert_associativity::<Falsity>(true);
        assert_associativity::<Conjunction>(true);
        assert_associativity::<MaterialNonImplication>(false);
        assert_associativity::<First>(true); // always first
        assert_associativity::<ConverseNonImplication>(false);
        assert_associativity::<Last>(true); // always last
        assert_associativity::<ExclusiveDisjunction>(true);
        assert_associativity::<Disjunction>(true);
        assert_associativity::<NonDisjunction>(false);
        assert_associativity::<LogicalBiconditional>(true);

        // left is Neg(LAST): F(F(a, b), c) = F(-b, c) = -c
        // right is Id(LAST): F(a, F(b, c)) = F(a, -c) = --c = c
        assert_associativity::<NotSecond>(false);
        assert_associativity::<ConverseImplication>(false);

        // left is Id(FIRST): F(F(a, b), c) = F(-a, c) = a
        // right is Neg(FIRST): F(a, F(b, c)) = F(a, -b) = -a
        assert_associativity::<NotFirst>(false);
        assert_associativity::<MaterialImplication>(false);
        assert_associativity::<NonConjunction>(false);
        assert_associativity::<Truth>(true);
    }
}

#[cfg(all(test, feature = "arbitrary"))]
mod prop_test {
    use proptest::prelude::*;

    use crate::formula::{Formula, FormulaParameters};

    use super::*;

    fn params() -> FormulaParameters<char> {
        FormulaParameters {
            variables: vec!['a', 'b', 'c', 'd'],
            leaf_var_weight: Some(10),
            use_dynamic: true,
            ..FormulaParameters::default()
        }
    }

    proptest! {
        // https://proptest-rs.github.io/proptest/proptest/tutorial/config.html
        #![proptest_config(ProptestConfig::with_cases(1000))]

        #[test]
        fn conjunction_identity(f in Formula::arbitrary_with(params())) {
            let id: Formula<_> = Conjunction.identity_element();
            assert_eq!(Conjunction.try_reduce([f.clone(), id.clone()]).unwrap(), f);
            assert_eq!(Conjunction.try_reduce([id, f.clone()]).unwrap(), f);
        }

        #[test]
        fn disjunction_identity(f in Formula::arbitrary_with(params())) {
            let id: Formula<_> = Disjunction.identity_element();
            assert_eq!(Disjunction.try_reduce([f.clone(), id.clone()]).unwrap(), f);
            assert_eq!(Disjunction.try_reduce([id, f.clone()]).unwrap(), f);
        }

        #[test]
        fn xor_identity(f in Formula::arbitrary_with(params())) {
            let id: Formula<_> = ExclusiveDisjunction.identity_element();
            assert_eq!(ExclusiveDisjunction.try_reduce([f.clone(), id.clone()]).unwrap(), f);
            assert_eq!(ExclusiveDisjunction.try_reduce([id, f.clone()]).unwrap(), f);
        }

        #[test]
        fn equiv_identity(f in Formula::arbitrary_with(params())) {
            let id: Formula<_> = LogicalBiconditional.identity_element();
            assert_eq!(LogicalBiconditional.try_reduce([f.clone(), id.clone()]).unwrap(), f);
            assert_eq!(LogicalBiconditional.try_reduce([id, f.clone()]).unwrap(), f);
        }
    }
}
