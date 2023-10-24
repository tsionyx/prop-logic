//! Describes logical operators (logical connectives),
//! their operations and mutual dependencies.
//!
//! <https://en.wikipedia.org/wiki/Logical_connective>
mod and;
mod converse_imply;
mod converse_nimply;
mod falsity;
mod id;
mod imply;
mod nand;
mod neg;
mod nimply;
mod nor;
mod or;
mod ordering;
mod proj;
mod truth;
pub mod truth_table;
mod xnor;
mod xor;

use std::{fmt, ops::Deref, sync::Arc};

use crate::formula::Formula;

pub use self::{
    and::Conjunction,
    falsity::Falsity,
    id::LogicalIdentity,
    neg::Negation,
    proj::{ProjectAndUnary, Projection},
    truth::Truth,
};

/// A function that accepts `ARITY` [truth values](https://en.wikipedia.org/wiki/Truth_value) as input
/// and produces a unique [truth value](https://en.wikipedia.org/wiki/Truth_value) as output.
///
/// <https://en.wikipedia.org/wiki/Truth_function>
pub trait TruthFunction<const ARITY: usize> {
    /// The way to express the [`TruthFunction`] as a boolean function of its arguments.
    ///
    /// It gets used later to generate the [`truth_table`].
    fn eval(values: [bool; ARITY]) -> bool;

    /// Returns a [callable object][Operation] which can
    /// represent the [`TruthFunction`] as a logical operation
    /// on simple boolean values.
    fn bool_evaluator() -> Operation<ARITY, bool>
    where
        Self: Sized + 'static,
    {
        Operation::new(Self::eval)
    }

    /// Create a [`Formula`] with this [`TruthFunction`].
    fn apply<T>(expr: [Formula<T>; ARITY]) -> Formula<T>;

    /// Returns a [callable object][Operation] which can
    /// be used to [apply][Self::apply] the [`TruthFunction`]
    /// to a number of [`Formula`]-s.
    fn formula_connector<T>() -> Operation<ARITY, Formula<T>> {
        Operation::new(Self::apply)
    }

    /// Create a `FunctionDescriptor` object combining
    /// the [`TruthFunction`] and [`Connective`] properties into a single variable.
    fn descriptor<Atom>() -> FunctionDescriptor<ARITY, Atom>
    where
        Self: Connective + Sized + 'static,
    {
        FunctionDescriptor {
            bool_evaluator: Self::bool_evaluator(),
            formula_connector: Self::formula_connector(),
            notation: Self::notation(),
            alternate_notations: Self::alternate_notations(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
/// Representation of a [`TruthFunction`] as an 'instance-level' variable,
/// opposed to the static ('class-level') typed description of it.
pub struct FunctionDescriptor<const ARITY: usize, Atom> {
    bool_evaluator: Operation<ARITY, bool>,
    formula_connector: Operation<ARITY, Formula<Atom>>,
    notation: FunctionNotation,
    alternate_notations: Option<Vec<FunctionNotation>>,
}

#[derive(Eq, PartialEq, Hash)]
/// Represents the most general form of
/// [homogeneous operation](https://en.wikipedia.org/wiki/Binary_operation).
///
/// It also works as a workaround emulating `impl Fn` behaviour for a struct.
pub struct Operation<const ARITY: usize, T> {
    func: fn([T; ARITY]) -> T,
}

impl<const ARITY: usize, T> fmt::Debug for Operation<ARITY, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-ary Operation", ARITY)
    }
}

impl<const ARITY: usize, T> Operation<ARITY, T> {
    fn new(func: fn([T; ARITY]) -> T) -> Self {
        Self { func }
    }
}

impl<const ARITY: usize, T> Deref for Operation<ARITY, T> {
    type Target = fn([T; ARITY]) -> T;

    fn deref(&self) -> &Self::Target {
        &self.func
    }
}

/// A [logical constant](https://en.wikipedia.org/wiki/Logical_constant)
/// that can be used to connect logical formulas.
pub trait Connective {
    /// Number of arguments which gets connected with this [`Connective`].
    ///
    /// <https://en.wikipedia.org/wiki/Arity>
    const ARITY: usize;

    /// The symbol (or name) most commonly used for the operation.
    fn notation() -> FunctionNotation;

    /// The set of alternate symbols (names) which can be used
    /// instead of the [main one][Self::notation].
    ///
    /// Could be helpful in parsing the expressions.
    ///
    /// Also,
    /// [the polish notation]( https://en.wikipedia.org/wiki/J%C3%B3zef_Maria_Boche%C5%84ski#Pr%C3%A9cis_de_logique_math%C3%A9matique)
    /// included.
    fn alternate_notations() -> Option<Vec<FunctionNotation>> {
        None
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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
                (Op::apply(formulas), Op::eval(assignment))
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
