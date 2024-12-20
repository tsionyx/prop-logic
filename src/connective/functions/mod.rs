mod binary;
pub mod falsity;
pub mod id;
pub mod neg;
pub mod ternary;
pub mod truth;

use super::{BoolFn, Connective, Formula, FunctionNotation, TruthFn};

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
        proj::{ProjectAndUnary, Projection},
        xnor::LogicalBiconditional,
        xor::ExclusiveDisjunction,
    },
    falsity::Falsity,
    id::LogicalIdentity,
    neg::Negation,
    truth::Truth,
};
