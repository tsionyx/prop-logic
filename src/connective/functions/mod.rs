pub mod and;
pub mod converse_imply;
pub mod converse_nimply;
pub mod falsity;
pub mod id;
pub mod imply;
pub mod nand;
pub mod neg;
pub mod nimply;
pub mod nor;
pub mod or;
pub mod proj;
pub mod ternary;
pub mod truth;
pub mod xnor;
pub mod xor;

use super::{BoolFn, Connective, Formula, FunctionNotation, TruthFn};

pub use self::{
    and::Conjunction,
    converse_imply::ConverseImplication,
    converse_nimply::ConverseNonImplication,
    falsity::Falsity,
    id::LogicalIdentity,
    imply::MaterialImplication,
    nand::NonConjunction,
    neg::Negation,
    nimply::MaterialNonImplication,
    nor::NonDisjunction,
    or::Disjunction,
    proj::{ProjectAndUnary, Projection},
    truth::Truth,
    xnor::LogicalBiconditional,
    xor::ExclusiveDisjunction,
};
