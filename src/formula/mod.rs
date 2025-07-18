//! Defines [propositional formula](https://en.wikipedia.org/wiki/Propositional_formula)
//!
//! which is a Boolean-valued
//! well-formed expression denoting a proposition and having as such
//! a [truth value](https://en.wikipedia.org/wiki/Truth_value).
mod assign;
mod eval;
mod general;
mod lit;
pub mod normal_forms;
mod ops;
mod var;

pub use self::{
    assign::Assignment,
    eval::Valuation,
    general::{
        connective::{AnyConnective, DynConnective},
        equivalences,
        formula::Formula,
        truth_table::FormulaTruthTable,
        RecursiveApplicationOrder,
    },
    lit::Literal,
    ops::{And, Equivalent, Implies, Not, Or, Xor},
    var::{Var, Variable},
};

#[cfg(feature = "arbitrary")]
pub use self::general::arbitrary::{BinaryWeights, Parameters as FormulaParameters};
