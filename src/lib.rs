//! # Propositional logic concepts
//!
//! - boolean algebra;
//! - boolean operators (logical connectives);
//! - truth tables;
//! - propositional expressions;
//! - normal forms;
//! - rules of inference.

// `use super::* and Enum::*` in tests
#![cfg_attr(test, allow(clippy::wildcard_imports, clippy::enum_glob_use))]
// using `expect` is almost always better, but `unwrap` still allowed in tests
#![cfg_attr(not(test), warn(clippy::unwrap_used))]

mod arity;
pub mod connective;
pub mod formula;
mod truth_table;
mod utils;

pub use self::{
    arity::{two_powers, two_powers_of_two_powers},
    formula::{Formula, Var, Variable},
    truth_table::{TruthTable, TruthTabled},
    utils::{
        dependent_array::{CheckedArray, CheckedStorage, Discriminant, SizedArray, VerifySize},
        dyn_eq::{AsDynCompare, DynCompare},
        operation::Operation,
        upcast::{Upcast, UpcastFrom},
        zst::{Void, Zst},
    },
};
