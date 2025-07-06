//! Special representations of [Formula][super::Formula]
//! optimized for various use cases.
//!
//! E.g.:
//! - [ANF][AlgebraicNormalForm] and [DNF][DisjunctiveNormalForm] are used in automated theorem proving;
//! - [CNF][ConjunctiveNormalForm] is used in the satisfyability problem.
pub(crate) mod anf;
pub(crate) mod cnf;
pub(crate) mod dnf;
pub(crate) mod error;
pub(crate) mod nnf;
pub(crate) mod traits;

// TODO: exotic normal form with the basis {0, '&', '=='}

pub use self::{
    anf::{NormalForm as AlgebraicNormalForm, Term as AlgebraicTerm},
    cnf::{Disjunct, NormalForm as ConjunctiveNormalForm},
    dnf::{Conjunct, NormalForm as DisjunctiveNormalForm},
    error::Error,
    nnf::NormalForm as NegationNormalForm,
    traits::NormalForm,
};
