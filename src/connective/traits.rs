use std::{
    collections::BTreeMap as Map,
    fmt::{self, Write},
    iter,
    ops::Deref,
    sync::Arc,
};

use itertools::Itertools as _;

use crate::{arity::two_powers, formula::Formula, utils::dependent_array::CheckedArray};

use super::truth_table::{Row, TruthTable};

#[auto_impl::auto_impl(&, Box)]
/// A function that accepts `ARITY` [`bool`] values as input
/// and produces a `bool` value as an output.
///
/// The [`ARITY`](https://en.wikipedia.org/wiki/Arity) is the number of arguments which this function accepts.
pub trait BoolFn<const ARITY: usize> {
    /// The way to express the [`BoolFn`] as a boolean function of its arguments.
    ///
    /// It gets used later to generate the [`truth_table`].
    fn eval(&self, values: [bool; ARITY]) -> bool;

    /// Generate a [truth table](https://en.wikipedia.org/wiki/Truth_table)
    /// for a [`BoolFn`] as the **key** (boolean arguments)-**value** (function result)
    /// ordered map.
    ///
    /// # Panics
    ///
    /// If a single point of cartesian product of `ARITY` bool values
    /// does not contain exactly `ARITY` values.
    /// This invariant should be guaranteed by the
    /// [itertools library][itertools::Itertools::multi_cartesian_product].
    fn get_truth_table(&self) -> TruthTable<ARITY>
    where
        two_powers::D: CheckedArray<ARITY>,
    {
        let table: Map<_, _> = iter::repeat([false, true])
            .take(ARITY)
            .multi_cartesian_product()
            .map(|assignment| {
                let assignment = assignment
                    .try_into()
                    .expect("The array size is guaranteed by Itertools::multi_cartesian_product");
                (assignment, self.eval(assignment))
            })
            .collect();

        let table = if ARITY == 0 {
            assert!(table.is_empty());
            let dummy_empty_array = [false; ARITY];
            let row: Row<ARITY> = (dummy_empty_array, self.eval(dummy_empty_array));
            vec![row]
        } else {
            table.into_iter().collect()
        };

        assert_eq!(table.len(), 1 << ARITY);
        table
            .try_into()
            .map_or_else(|_| panic!("Size checked before"), TruthTable::new)
    }
}

/// A function that accepts `ARITY` [truth values](https://en.wikipedia.org/wiki/Truth_value) as input
/// and produces a unique [truth value](https://en.wikipedia.org/wiki/Truth_value) as output.
///
/// The [`ARITY`](https://en.wikipedia.org/wiki/Arity) is the number of arguments which this function accepts.
///
/// <https://en.wikipedia.org/wiki/Truth_function>
pub trait TruthFn<const ARITY: usize>: BoolFn<ARITY> {
    /// Every [`TruthFn`] is essentially a bunch of _static_ functions
    /// without the need to create an _instance_. So, as a rule of thumb,
    /// any instantiated type should have exactly one value (unit type, isomorphic to `()`).
    ///
    /// But, in order to use the [`TruthFn`] in a dynamic context
    /// we have to create some 'dummy' instance.
    /// Usually, the [`Default`] trait can be used instead of the [`TruthFn::init`],
    /// but using [`Default`] as a supertrait here disables the use of `Box<dyn TruthFn>`
    /// because of the `Default: Sized` requirement.
    ///
    /// Unfortunately it is not possible now in stable rust
    /// to ensure the generic type is ZST, so whenever you use
    /// the [`TruthFn`] and want to ensure it has zero size,
    /// you have to:
    /// 1. Require that the generic [`TruthFn`] is [`ZST`][crate::utils::Zst].
    ///    This is not used as a supertrait here deliberately to allow to use `dyn TruthFn`,
    ///    (as the `Zst` trait contains an associated constant preventing the dynamic object to create).
    /// 2. Use the [`Zst::ASSERT_ZST`][crate::utils::Zst::ASSERT_ZST] in your function accepting
    ///    generic `TruthFn` to force the compiler to do `size_of` check.
    fn init() -> Self
    where
        Self: Sized;

    /// Returns a [callable object][Operation] which can
    /// represent the [`BoolFn`] as a logical operation
    /// on simple boolean values.
    fn bool_evaluator(self) -> Operation<ARITY, bool>
    where
        Self: Sized + 'static,
    {
        Operation::new(Box::new(move |args| self.eval(args)))
    }

    /// Create a [`Formula`] with this [`TruthFn`].
    fn apply<T>(&self, expr: [Formula<T>; ARITY]) -> Formula<T>
    where
        Self: Sized;

    /// Returns a [callable object][Operation] which can
    /// be used to [apply][Self::apply] the [`TruthFn`]
    /// to a number of [`Formula`]-s.
    fn formula_connector<T>(self) -> Operation<ARITY, Formula<T>>
    where
        Self: Sized + 'static,
    {
        Operation::new(Box::new(move |args| self.apply(args)))
    }
}

/// A [logical constant](https://en.wikipedia.org/wiki/Logical_constant)
/// that can be used to connect logical formulas.
///
/// Its main usage is to convert to/from string representation of a formula.
///
/// Every [`Connective`] should necessary be a [`TruthFn`],
/// but not the other way around. E.g. the [`LogicalIdentity`]
/// could not be a connective since it has no string representation
/// and cannot be used in formulas as a separate entity.
pub trait Connective<const ARITY: usize>: TruthFn<ARITY> {
    /// The symbol (or name) most commonly used for the operation.
    fn notation(&self) -> FunctionNotation;

    /// The set of alternate symbols (names) which can be used
    /// instead of the [main one][Self::notation].
    ///
    /// Could be helpful in parsing the expressions.
    ///
    /// Also,
    /// [the polish notation]( https://en.wikipedia.org/wiki/J%C3%B3zef_Maria_Boche%C5%84ski#Pr%C3%A9cis_de_logique_math%C3%A9matique)
    /// included.
    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        None
    }
}

#[derive(Debug, Clone)]
/// Name or notation of the function.
pub enum FunctionNotation {
    /// Symbolic representation of a functions as an operator.
    /// The common usage of this symbol:
    /// - as [prefix](https://en.wikipedia.org/wiki/Polish_notation) operator form for unary functions;
    /// - as [infix](https://en.wikipedia.org/wiki/Infix_notation) operator form for binary functions.
    Symbol(char),

    /// The name of the function (usually as the latin name).
    Name(Arc<str>),
    // TODO: more categories like ASCII, Polish notation, scheme element name, etc.
}

impl From<char> for FunctionNotation {
    fn from(value: char) -> Self {
        Self::Symbol(value)
    }
}

impl From<String> for FunctionNotation {
    fn from(value: String) -> Self {
        Self::Name(value.into())
    }
}

impl From<&'static str> for FunctionNotation {
    fn from(value: &'static str) -> Self {
        Self::Name(value.into())
    }
}

impl fmt::Display for FunctionNotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Name(name) => f.write_str(name),
            Self::Symbol(s) => f.write_char(*s),
        }
    }
}

/// Represents the most general form of
/// [homogeneous operation](https://en.wikipedia.org/wiki/Binary_operation).
///
/// It also works as a workaround emulating `impl Fn` behaviour
/// by allowing to 'call' it through [`Deref`]-ing.
pub struct Operation<const ARITY: usize, T> {
    func: Box<dyn Fn([T; ARITY]) -> T>,
}

impl<const ARITY: usize, T> fmt::Debug for Operation<ARITY, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{ARITY}-ary Operation")
    }
}

impl<const ARITY: usize, T> Operation<ARITY, T> {
    fn new(func: Box<dyn Fn([T; ARITY]) -> T>) -> Self {
        Self { func }
    }
}

impl<const ARITY: usize, T> Deref for Operation<ARITY, T>
where
    T: 'static,
{
    type Target = dyn Fn([T; ARITY]) -> T;

    fn deref(&self) -> &Self::Target {
        &self.func
    }
}
