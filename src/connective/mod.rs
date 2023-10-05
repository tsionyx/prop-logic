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

    /// Returns a [callable object][OperatorEvaluator] which can
    /// be called to [evaluate][Self::eval] the result of applying the [`TruthFunction`].
    fn evaluator() -> OperatorEvaluator<ARITY>
    where
        Self: Sized + 'static,
    {
        OperatorEvaluator::with_connective::<Self>()
    }

    /// Create a [`Formula`] with this [`TruthFunction`].
    fn apply<T>(expr: [Formula<T>; ARITY]) -> Formula<T>;
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

/// Workaround emulating `impl Fn` behaviour
/// for the [`TruthFunction`].
pub struct OperatorEvaluator<const N: usize> {
    closure: Box<dyn Fn([bool; N]) -> bool>,
}

impl<const N: usize> fmt::Debug for OperatorEvaluator<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "OperatorEvaluator for {}-arity predicate", N)
    }
}

impl<const N: usize> OperatorEvaluator<N> {
    fn new(closure: Box<dyn Fn([bool; N]) -> bool>) -> Self {
        Self { closure }
    }

    fn with_connective<O>() -> Self
    where
        O: TruthFunction<N> + 'static,
    {
        Self::new(Box::new(O::eval))
    }
}

impl<const N: usize> Deref for OperatorEvaluator<N> {
    type Target = dyn Fn([bool; N]) -> bool;

    fn deref(&self) -> &Self::Target {
        &*self.closure
    }
}
