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
    anf::NormalForm as AlgebraicNormalForm, cnf::NormalForm as ConjunctiveNormalForm,
    dnf::NormalForm as DisjunctiveNormalForm, error::Error, nnf::NormalForm as NegationNormalForm,
    traits::NormalForm,
};

/// Combination of items connected with
/// the [AND operation][crate::connective::Conjunction].
pub type Conjunct<T> = crate::connective::Series<T, crate::connective::Conjunction>;

impl<T> Conjunct<T> {
    /// The empty series of conjuncts forms a tautology.
    pub fn tautology() -> Self {
        Self::new(None)
    }

    /// No conjuncts is a tautology.
    pub fn is_tautology(&self) -> bool {
        self.as_ref().is_empty()
    }
}

/// Combination of items connected with
/// the [OR operation][crate::connective::Disjunction].
pub type Disjunct<T> = crate::connective::Series<T, crate::connective::Disjunction>;

impl<T> Disjunct<T> {
    /// The empty series of disjuncts forms a contradiction.
    pub fn contradiction() -> Self {
        Self::new(None)
    }

    /// No disjuncts is a contradiction.
    pub fn is_contradiction(&self) -> bool {
        self.as_ref().is_empty()
    }
}
