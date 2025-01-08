use crate::connective::TruthFn as _;

use super::{
    super::{
        connective::{AnyConnective, DynConnective},
        formula::Formula,
    },
    RewritingRule,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Remove the constants from the [`Formula`] by applying the [`TruthFn::compose`].
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
    fn minimize(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        match formula.into() {
            AnyConnective::Nullary(operator) => operator
                .connective
                .fold([])
                .map_err(|_| Formula::Other(AnyConnective::Nullary(operator.map(Box::new)))),
            AnyConnective::Unary(DynConnective {
                connective,
                operands,
            }) => connective.fold(operands).map_err(|formulas| {
                Formula::Other(AnyConnective::Unary(DynConnective {
                    connective,
                    operands: formulas.map(Box::new),
                }))
            }),
            AnyConnective::Binary(DynConnective {
                connective,
                operands,
            }) => connective.fold(operands).map_err(|formulas| {
                Formula::Other(AnyConnective::Binary(DynConnective {
                    connective,
                    operands: formulas.map(Box::new),
                }))
            }),
        }
    }
}
