//! Describes logical operators (logical connectives),
//! their operations and mutual dependencies.
//!
//! <https://en.wikipedia.org/wiki/Logical_connective>
mod evaluation;
pub(crate) mod functions;
mod ops;
mod ordering;
mod priority;
mod properties;
mod storage;
mod traits;
mod truth_table;

use crate::formula::Formula;

pub use self::{
    evaluation::Evaluation,
    functions::*,
    ops::{Associativity, Commutativity, Converse, Negate},
    priority::{Prioritized, Priority},
    properties::{is_basis, is_complete, BoolFnExt},
    storage::{AllFunctions, BINARY_FUNCTIONS, NULLARY_FUNCTIONS, UNARY_FUNCTIONS},
    traits::{BoolFn, Connective, FormulaComposer, FunctionNotation, Reducible, TruthFn},
    truth_table::TruthTable,
};

#[allow(path_statements, clippy::no_effect)]
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
    use itertools::Itertools as _;

    use crate::{arity::two_powers, formula::Valuation, utils::dependent_array::CheckedArray};

    use super::*;

    #[allow(clippy::needless_pass_by_value)]
    fn apply_and_compose_is_equivalent<const ARITY: usize, F>(f: F)
    where
        F: TruthFn<ARITY> + FormulaComposer<ARITY, ()>,
        two_powers::D: CheckedArray<ARITY>,
    {
        let truth_table = f.get_truth_table().into_iter();
        let eval_variants = truth_table.map(|(assignment, eval)| {
            let formulas = assignment
                .into_iter()
                .map(Formula::<()>::TruthValue)
                .collect_vec();
            let formulas = formulas
                .try_into()
                .expect("Cartesian product ensures the length of the tuple to be equal to ARITY");

            (f.compose(formulas), eval)
        });

        let empty_interpretation = Valuation::new();
        for (fully_interpreted_formula, expected_eval) in eval_variants {
            eprintln!("{fully_interpreted_formula:?} -> {expected_eval:?}");
            if let Formula::TruthValue(val) =
                fully_interpreted_formula.interpret(&empty_interpretation)
            {
                assert_eq!(val, expected_eval);
            } else {
                panic!("The formula was not fully reduced");
            }
        }
    }

    #[test]
    fn eval_is_sync_with_apply() {
        // nullary
        apply_and_compose_is_equivalent::<0, _>(Falsity);
        apply_and_compose_is_equivalent::<0, _>(Truth);

        // unary
        apply_and_compose_is_equivalent::<1, _>(Falsity);
        apply_and_compose_is_equivalent::<1, _>(LogicalIdentity);
        apply_and_compose_is_equivalent::<1, _>(Negation);
        apply_and_compose_is_equivalent::<1, _>(Truth);

        // binary
        apply_and_compose_is_equivalent::<2, _>(Falsity);
        apply_and_compose_is_equivalent::<2, _>(Conjunction);
        apply_and_compose_is_equivalent::<2, _>(MaterialNonImplication);
        apply_and_compose_is_equivalent::<2, _>(Projection::<0>);
        apply_and_compose_is_equivalent::<2, _>(ConverseNonImplication);
        apply_and_compose_is_equivalent::<2, _>(Projection::<1>);
        apply_and_compose_is_equivalent::<2, _>(ExclusiveDisjunction);
        apply_and_compose_is_equivalent::<2, _>(Disjunction);

        apply_and_compose_is_equivalent::<2, _>(NonDisjunction);
        apply_and_compose_is_equivalent::<2, _>(LogicalBiconditional);
        apply_and_compose_is_equivalent::<2, _>(ProjectAndUnary::<1, Negation>::new());
        apply_and_compose_is_equivalent::<2, _>(ConverseImplication);
        apply_and_compose_is_equivalent::<2, _>(ProjectAndUnary::<0, Negation>::new());
        apply_and_compose_is_equivalent::<2, _>(MaterialImplication);
        apply_and_compose_is_equivalent::<2, _>(NonConjunction);
        apply_and_compose_is_equivalent::<2, _>(Truth);
    }
}
