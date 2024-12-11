//! Special representations of [Formula][super::Formula]
//! optimized for various use cases.
//!
//! E.g.:
//! - [ANF][AlgebraicNormalForm] and [DNF][DisjunctivNormalForm] are used in automated theorem proving;
//! - [CNF][ConjunctivNormalForm] is used in the satisfyability problem.
pub(crate) mod anf;
pub(crate) mod cnf;
pub(crate) mod dnf;
pub(crate) mod nnf;

// TODO: exotic normal form with the basis {0, '&', '=='}

pub use self::{
    anf::{NormalForm as AlgebraicNormalForm, Term as AlgebraicTerm},
    cnf::{Disjunct, NormalForm as ConjunctivNormalForm},
    dnf::{Conjunct, NormalForm as DisjunctivNormalForm},
    nnf::NormalForm as NegationNormalForm,
};
