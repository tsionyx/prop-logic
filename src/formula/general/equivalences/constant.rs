use super::{
    super::{
        connective::{AnyConnective, DynConnective},
        formula::Formula,
    },
    RewritingRule,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Remove the constants from the [`Formula`] by applying the [`TruthFn::try_reduce`].
///
/// This automatically covers the most evident laws, e.g:
/// - identity law:
///   - p ∧ ⊤ ≡ p
///   - p ∨ ⊥ ≡ p
/// - domination law:
///   - p ∨ ⊤ ≡ ⊤
///   - p ∧ ⊥ ≡ ⊥
pub struct EliminateConstants;

impl<T> RewritingRule<T> for EliminateConstants {
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        let is_dynamic = formula.is_dynamic();
        match AnyConnective::from(formula) {
            AnyConnective::Nullary(operator) => operator
                .connective
                .try_reduce([])
                .map_err(|_| AnyConnective::Nullary(operator)),
            AnyConnective::Unary(DynConnective {
                connective,
                operands,
            }) => connective.try_reduce(operands).map_err(|formulae| {
                AnyConnective::Unary(DynConnective {
                    connective,
                    operands: formulae,
                })
            }),
            AnyConnective::Binary(DynConnective {
                connective,
                operands,
            }) => connective.try_reduce(operands).map_err(|formulae| {
                AnyConnective::Binary(DynConnective {
                    connective,
                    operands: formulae,
                })
            }),
        }
        .map_err(|any| any.map(Box::new).into_formula(is_dynamic))
    }
}
