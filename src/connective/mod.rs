//! Describes logical operators (logical connectives),
//! their operations and mutual dependencies.
//!
//! <https://en.wikipedia.org/wiki/Logical_connective>
mod functions;
mod ops;
mod ordering;
mod storage;
pub mod truth_table;

use std::{collections::BTreeMap, fmt, iter, ops::Deref, sync::Arc};

use itertools::Itertools as _;

use crate::formula::Formula;

pub use self::{
    functions::*,
    ops::{Converse, Negate},
};

#[allow(path_statements)]
const _ASSERT_ZST: () = {
    use crate::utils::Zst;

    // nullary and unary
    Falsity::ASSERT_ZST;
    LogicalIdentity::ASSERT_ZST;
    Negation::ASSERT_ZST;
    Truth::ASSERT_ZST;

    // then binary
    Conjunction::ASSERT_ZST;
    MaterialNonImplication::ASSERT_ZST;
    Projection::<0>::ASSERT_ZST;
    ConverseNonImplication::ASSERT_ZST;
    Projection::<1>::ASSERT_ZST;
    ExclusiveDisjunction::ASSERT_ZST;
    Disjunction::ASSERT_ZST;
    NonDisjunction::ASSERT_ZST;
    LogicalBiconditional::ASSERT_ZST;
    ProjectAndUnary::<1, Negation>::ASSERT_ZST;
    ConverseImplication::ASSERT_ZST;
    ProjectAndUnary::<0, Negation>::ASSERT_ZST;
    MaterialImplication::ASSERT_ZST;
    NonConjunction::ASSERT_ZST;
};

/// A function that accepts `ARITY` [truth values](https://en.wikipedia.org/wiki/Truth_value) as input
/// and produces a unique [truth value](https://en.wikipedia.org/wiki/Truth_value) as output.
///
/// The [`ARITY`](https://en.wikipedia.org/wiki/Arity) is the number of arguments which this function accepts.
///
/// <https://en.wikipedia.org/wiki/Truth_function>
pub trait TruthFunction<const ARITY: usize> {
    /// Every [`TruthFunction`] is essentially a bunch of _static_ functions
    /// without the need to create an _instance_. So, as a rule of thumb,
    /// any instantiated type should have exactly one value (unit type, isomorphic to `()`).
    ///
    /// But, in order to use the [`TruthFunction`] in a dynamic context
    /// we have to create some 'dummy' instance.
    ///
    /// Unfortunately it is not possible now in stable rust
    /// to ensure the generic type is ZST, so whenever you use
    /// the [`TruthFunction`] and want to ensure it has zero size,
    /// you have to:
    /// 1. Require that the generic [`TruthFunction`] is [`ZST`][crate::utils::Zst].
    ///    This is not used as a supertrait here deliberately to allow to use `dyn TruthFunction`,
    ///    (as the `Zst` trait contains an associated constant preventing the dynamic object to create).
    /// 2. Use the [`Zst::ASSERT_ZST`][crate::utils::Zst::ASSERT_ZST] in your function accepting
    ///    generic `TruthFunction` to force the compiler to do `size_of` check.
    fn init() -> Self
    where
        Self: Sized;

    /// The way to express the [`TruthFunction`] as a boolean function of its arguments.
    ///
    /// It gets used later to generate the [`truth_table`].
    fn eval(&self, values: [bool; ARITY]) -> bool;

    /// Returns a [callable object][Operation] which can
    /// represent the [`TruthFunction`] as a logical operation
    /// on simple boolean values.
    fn bool_evaluator(&self) -> Operation<ARITY, bool>
    where
        Self: Copy + 'static,
    {
        let self_ = *self;
        Operation::new(Box::new(move |args| self_.eval(args)))
    }

    /// Create a [`Formula`] with this [`TruthFunction`].
    fn apply<T>(&self, expr: [Formula<T>; ARITY]) -> Formula<T>
    where
        Self: Sized;

    /// Returns a [callable object][Operation] which can
    /// be used to [apply][Self::apply] the [`TruthFunction`]
    /// to a number of [`Formula`]-s.
    fn formula_connector<T>(&self) -> Operation<ARITY, Formula<T>>
    where
        Self: Copy + 'static,
    {
        let self_ = *self;
        Operation::new(Box::new(move |args| self_.apply(args)))
    }

    /// Generate a [truth table](https://en.wikipedia.org/wiki/Truth_table)
    /// for a [`TruthFunction`] as the **key** (boolean arguments)-**value** (function result)
    /// ordered map.
    ///
    /// # Panics
    ///
    /// If a single point of cartesian product of `ARITY` bool values
    /// does not contain exactly `ARITY` values.
    /// This invariant should be guaranteed by the
    /// [itertools library][itertools::Itertools::multi_cartesian_product].
    fn get_truth_table(&self) -> BTreeMap<[bool; ARITY], bool> {
        let table: BTreeMap<_, _> = iter::repeat([false, true])
            .take(ARITY)
            .multi_cartesian_product()
            .map(|assignment| {
                let assignment = assignment.try_into().unwrap();
                (assignment, self.eval(assignment))
            })
            .collect();

        if ARITY > 0 {
            assert_eq!(table.len(), 1 << ARITY);
            table
        } else {
            assert!(table.is_empty());
            assert_eq!(ARITY, 0);
            let dummy_empty_array = [false; ARITY];
            iter::once((dummy_empty_array, self.eval(dummy_empty_array))).collect()
        }
    }
}

/// A [logical constant](https://en.wikipedia.org/wiki/Logical_constant)
/// that can be used to connect logical formulas.
///
/// Its main usage is to convert to/from string representation of a formula.
pub trait Connective<const ARITY: usize>: TruthFunction<ARITY> {
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
        write!(f, "{}-ary Operation", ARITY)
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

#[cfg(test)]
mod tests {
    use std::iter;

    use itertools::Itertools as _;

    use super::*;
    use crate::formula::Valuation;

    pub(super) fn apply_and_eval_is_equivalent<Op, const ARITY: usize>()
    where
        Op: TruthFunction<ARITY>,
    {
        let eval_variants = iter::repeat([false, true])
            .take(ARITY)
            .multi_cartesian_product()
            .map(|assignment| {
                let formulas = assignment
                    .iter()
                    .copied()
                    .map(Formula::<()>::TruthValue)
                    .collect_vec();
                let formulas = formulas.try_into().expect(
                    "Cartesian product ensures the length of the tuple to be equal to ARITY",
                );

                let assignment = assignment.try_into().expect(
                    "Cartesian product ensures the length of the tuple to be equal to ARITY",
                );
                (Op::init().apply(formulas), Op::init().eval(assignment))
            });

        let empty_interpretation = Valuation::new();
        for (fully_interpreted_formula, expected_eval) in eval_variants {
            eprintln!("{:?} -> {:?}", fully_interpreted_formula, expected_eval);
            if let Formula::TruthValue(val) =
                fully_interpreted_formula.interpret(&empty_interpretation)
            {
                assert_eq!(val, expected_eval);
            } else {
                panic!("The formula was not fully reduced");
            }
        }
    }
}
