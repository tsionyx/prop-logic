use std::{collections::BTreeMap as Map, iter};

use itertools::Itertools as _;

use crate::{
    arity::two_powers,
    formula::Formula,
    utils::{dependent_array::CheckedArray, operation::Operation, upcast::UpcastFrom},
};

use super::{
    evaluation::Evaluation,
    notation::FunctionNotation,
    truth_table::{Row, TruthTable},
};

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

impl<'a, const ARITY: usize, T: BoolFn<ARITY> + 'a> UpcastFrom<T> for dyn BoolFn<ARITY> + 'a {
    fn up_from(value: &T) -> &(dyn BoolFn<ARITY> + 'a) {
        value
    }
    fn up_from_mut(value: &mut T) -> &mut (dyn BoolFn<ARITY> + 'a) {
        value
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
}

#[auto_impl::auto_impl(&, Box)]
/// Enables the ability for the boolean connective
/// to simplify a propositional statement by taking
/// into account the partial knowledge of its arguments.
///
/// This is can also be viewed as
/// [short-circuit evaluation](https://en.wikipedia.org/wiki/Short-circuit_evaluation).
pub trait Reducible<const ARITY: usize, T>: BoolFn<ARITY> {
    /// Try to reduce the statement.
    ///
    /// It is a more general version of [`BoolFn::eval`]
    /// which can reduce a propositional statement of many arguments
    /// to the function of its single argument.
    fn try_reduce(&self, values: [Evaluation<T>; ARITY]) -> Option<Evaluation<T>>;
}

/// Enables the ability for to combine boolean formulas into a single formula.
pub trait FormulaComposer<const ARITY: usize, T>: Reducible<ARITY, Formula<T>> {
    /// Compose a [`Formula`] from other [`Formula`]-s using self as a connective.
    fn compose(&self, formulas: [Formula<T>; ARITY]) -> Formula<T>;

    /// Returns a [callable object][Operation] which can
    /// be used to [compose][Self::compose] a number of [`Formula`]-s.
    fn formula_connector(self) -> Operation<ARITY, Formula<T>>
    where
        Self: Sized + 'static,
    {
        Operation::new(Box::new(move |args| self.compose(args)))
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
