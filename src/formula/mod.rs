//! Defines [propositional formula](https://en.wikipedia.org/wiki/Propositional_formula)
//!
//! which is a Boolean-valued
//! well-formed expression denoting a proposition and having as such
//! a [truth value](https://en.wikipedia.org/wiki/Truth_value).
mod atom;
mod eval;
mod general;
mod lit;
pub mod normal_forms;
mod ops;
mod var;

pub use self::{
    atom::{Assignment, Atom},
    eval::Valuation,
    general::{
        connective::{AnyConnective, DynConnective},
        equivalences,
        formula::{Directed, Formula},
        truth_table::FormulaTruthTable,
    },
    lit::{Lit, Literal},
    ops::{And, Equivalent, Implies, Not, Or, Xor},
    var::{Var, Variable},
};
