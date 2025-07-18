mod binary;
pub mod falsity;
pub mod id;
pub mod neg;
pub mod ternary;
pub mod truth;

pub use self::{
    binary::{
        and::{Conjunction, ConjunctionAny},
        converse_imply::ConverseImplication,
        converse_nimply::ConverseNonImplication,
        imply::MaterialImplication,
        nand::NonConjunction,
        nimply::MaterialNonImplication,
        nor::NonDisjunction,
        or::{Disjunction, DisjunctionAny},
        proj::{First, Last, NotFirst, NotSecond, ProjectAndUnary, Projection},
        xnor::{AllEquivalent, EquivalentAny, LogicalBiconditional},
        xor::{ExclusiveDisjunction, ExclusiveDisjunctionAny},
    },
    falsity::Falsity,
    id::LogicalIdentity,
    neg::Negation,
    truth::Truth,
};
