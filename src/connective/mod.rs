//! Describes logical operators (logical connectives),
//! their operations and mutual dependencies.
//!
//! <https://en.wikipedia.org/wiki/Logical_connective>
mod functions;
mod ops;
mod ordering;
mod storage;
mod traits;
pub mod truth_table;

use crate::formula::Formula;

pub use self::{
    functions::*,
    ops::{Converse, Negate},
    storage::{AllFunctions, BINARY_FUNCTIONS, NULLARY_FUNCTIONS, UNARY_FUNCTIONS},
    traits::{Connective, FunctionNotation, Operation, TruthFunction},
};

#[allow(path_statements)]
const _ASSERT_ZST: () = {
    use crate::utils::Zst;

    // nullary and unary
    Falsity::ASSERT_ZST;
    LogicalIdentity::ASSERT_ZST;
    Negation::ASSERT_ZST;
    Truth::ASSERT_ZST;

    // then binary
    Conjunction::ASSERT_ZST;
    MaterialNonImplication::ASSERT_ZST;
    Projection::<0>::ASSERT_ZST;
    ConverseNonImplication::ASSERT_ZST;
    Projection::<1>::ASSERT_ZST;
    ExclusiveDisjunction::ASSERT_ZST;
    Disjunction::ASSERT_ZST;
    NonDisjunction::ASSERT_ZST;
    LogicalBiconditional::ASSERT_ZST;
    ProjectAndUnary::<1, Negation>::ASSERT_ZST;
    ConverseImplication::ASSERT_ZST;
    ProjectAndUnary::<0, Negation>::ASSERT_ZST;
    MaterialImplication::ASSERT_ZST;
    NonConjunction::ASSERT_ZST;
};

#[cfg(test)]
mod tests {
    use std::iter;

    use itertools::Itertools as _;

    use super::*;
    use crate::formula::Valuation;

    pub(super) fn apply_and_eval_is_equivalent<Op, const ARITY: usize>()
    where
        Op: TruthFunction<ARITY>,
    {
        let eval_variants = iter::repeat([false, true])
            .take(ARITY)
            .multi_cartesian_product()
            .map(|assignment| {
                let formulas = assignment
                    .iter()
                    .copied()
                    .map(Formula::<()>::TruthValue)
                    .collect_vec();
                let formulas = formulas.try_into().expect(
                    "Cartesian product ensures the length of the tuple to be equal to ARITY",
                );

                let assignment = assignment.try_into().expect(
                    "Cartesian product ensures the length of the tuple to be equal to ARITY",
                );
                (Op::init().apply(formulas), Op::init().eval(assignment))
            });

        let empty_interpretation = Valuation::new();
        for (fully_interpreted_formula, expected_eval) in eval_variants {
            eprintln!("{:?} -> {:?}", fully_interpreted_formula, expected_eval);
            if let Formula::TruthValue(val) =
                fully_interpreted_formula.interpret(&empty_interpretation)
            {
                assert_eq!(val, expected_eval);
            } else {
                panic!("The formula was not fully reduced");
            }
        }
    }
}
