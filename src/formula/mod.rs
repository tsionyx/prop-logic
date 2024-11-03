//! Defines [propositional formula](https://en.wikipedia.org/wiki/Propositional_formula)
//! which is a Boolean-valued
//! well-formed expression denoting a proposition and having as such
//! a [truth value](https://en.wikipedia.org/wiki/Truth_value).
mod atom;
mod connective;
mod eval;
#[allow(clippy::module_inception)]
mod formula;
mod ops;
mod var;

pub use self::{
    atom::{Atom, AtomValue},
    connective::DynOperator,
    eval::Valuation,
    formula::Formula,
    ops::{And, Equivalent, Implies, Or, Xor},
    var::{Var, Variable},
};
