//! Describes logical operators (logical connectives),
//! their operations and mutual dependencies.
//!
//! <https://en.wikipedia.org/wiki/Logical_connective>
mod evaluation;
pub(crate) mod functions;
mod notation;
mod ops;
mod ordering;
mod priority;
mod properties;
mod storage;
mod traits;
mod truth_table;

pub use self::{
    evaluation::Evaluable,
    functions::*,
    notation::FunctionNotation,
    ops::{Associativity, Commutativity, Converse, Negate},
    priority::{Prioritized, Priority},
    properties::{is_basis, is_complete, BoolFnExt},
    storage::{AllFunctions, StoredBoolFn, BINARY_FUNCTIONS, NULLARY_FUNCTIONS, UNARY_FUNCTIONS},
    traits::{BoolFn, Connective, InitFn, TruthFn, TruthFnConnector},
    truth_table::FixedTruthTable,
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
    First::ASSERT_ZST;
    ConverseNonImplication::ASSERT_ZST;
    Last::ASSERT_ZST;
    ExclusiveDisjunction::ASSERT_ZST;
    Disjunction::ASSERT_ZST;
    NonDisjunction::ASSERT_ZST;
    LogicalBiconditional::ASSERT_ZST;
    NotSecond::ASSERT_ZST;
    ConverseImplication::ASSERT_ZST;
    NotFirst::ASSERT_ZST;
    MaterialImplication::ASSERT_ZST;
    NonConjunction::ASSERT_ZST;
};

#[cfg(test)]
mod tests {
    use itertools::Itertools as _;

    use crate::{
        arity::two_powers,
        formula::{Formula, Valuation},
        utils::dependent_array::CheckedArray,
    };

    use super::*;

    #[allow(clippy::needless_pass_by_value)]
    fn apply_and_compose_is_equivalent<const ARITY: usize, F>(f: F)
    where
        F: TruthFn<ARITY, Formula<()>> + BoolFn<ARITY>,
        two_powers::D: CheckedArray<ARITY>,
    {
        use crate::truth_table::TruthTabled as _;

        let truth_table = f.get_truth_table().into_iter();
        let eval_variants = truth_table.map(|(assignment, eval)| {
            let formulas = assignment
                .into_iter()
                .map(Formula::<()>::truth)
                .collect_vec();
            let formulas = formulas
                .try_into()
                .expect("Cartesian product ensures the length of the tuple to be equal to ARITY");

            (f.compose(formulas), eval)
        });

        let empty_interpretation: Valuation<()> = Valuation::new();
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

        apply_and_compose_is_equivalent::<0, _>(DisjunctionAny);
        apply_and_compose_is_equivalent::<0, _>(ExclusiveDisjunctionAny);
        apply_and_compose_is_equivalent::<0, _>(ConjunctionAny);
        apply_and_compose_is_equivalent::<0, _>(AllEquivalent);

        apply_and_compose_is_equivalent::<0, _>(Truth);

        // unary
        apply_and_compose_is_equivalent::<1, _>(Falsity);
        apply_and_compose_is_equivalent::<1, _>(LogicalIdentity);

        apply_and_compose_is_equivalent::<1, _>(DisjunctionAny);
        apply_and_compose_is_equivalent::<1, _>(ConjunctionAny);
        apply_and_compose_is_equivalent::<1, _>(AllEquivalent);
        apply_and_compose_is_equivalent::<1, _>(ExclusiveDisjunctionAny);

        apply_and_compose_is_equivalent::<1, _>(Negation);
        apply_and_compose_is_equivalent::<1, _>(Truth);

        // binary
        apply_and_compose_is_equivalent::<2, _>(Falsity);
        apply_and_compose_is_equivalent::<2, _>(Conjunction);
        apply_and_compose_is_equivalent::<2, _>(ConjunctionAny);
        apply_and_compose_is_equivalent::<2, _>(MaterialNonImplication);
        apply_and_compose_is_equivalent::<2, _>(First {});
        apply_and_compose_is_equivalent::<2, _>(ConverseNonImplication);
        apply_and_compose_is_equivalent::<2, _>(Last {});
        apply_and_compose_is_equivalent::<2, _>(ExclusiveDisjunction);
        apply_and_compose_is_equivalent::<2, _>(ExclusiveDisjunctionAny);
        apply_and_compose_is_equivalent::<2, _>(Disjunction);
        apply_and_compose_is_equivalent::<2, _>(DisjunctionAny);

        apply_and_compose_is_equivalent::<2, _>(NonDisjunction);
        apply_and_compose_is_equivalent::<2, _>(LogicalBiconditional);
        apply_and_compose_is_equivalent::<2, _>(AllEquivalent);
        apply_and_compose_is_equivalent::<2, _>(NotSecond::new());
        apply_and_compose_is_equivalent::<2, _>(ConverseImplication);
        apply_and_compose_is_equivalent::<2, _>(NotFirst::new());
        apply_and_compose_is_equivalent::<2, _>(MaterialImplication);
        apply_and_compose_is_equivalent::<2, _>(NonConjunction);
        apply_and_compose_is_equivalent::<2, _>(Truth);
    }
}
