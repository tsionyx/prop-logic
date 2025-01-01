mod binary;
pub mod falsity;
pub mod id;
pub mod neg;
pub mod ternary;
pub mod truth;

pub use self::{
    binary::{
        and::Conjunction,
        converse_imply::ConverseImplication,
        converse_nimply::ConverseNonImplication,
        imply::MaterialImplication,
        nand::NonConjunction,
        nimply::MaterialNonImplication,
        nor::NonDisjunction,
        or::Disjunction,
        proj::{First, Last, NotFirst, NotSecond, ProjectAndUnary, Projection},
        xnor::LogicalBiconditional,
        xor::ExclusiveDisjunction,
    },
    falsity::Falsity,
    id::LogicalIdentity,
    neg::Negation,
    truth::Truth,
};
