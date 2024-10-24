pub(crate) mod and;
pub(crate) mod converse_imply;
pub(crate) mod converse_nimply;
pub(crate) mod falsity;
pub(crate) mod id;
pub(crate) mod imply;
pub(crate) mod nand;
pub(crate) mod neg;
pub(crate) mod nimply;
pub(crate) mod nor;
pub(crate) mod or;
pub(crate) mod proj;
pub(crate) mod truth;
pub(crate) mod xnor;
pub(crate) mod xor;

use super::{Connective, Formula, FunctionNotation, TruthFunction};

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
