use super::{super::formula::Formula, RewritingRule};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Convert [`Formula::Other`] into the [`Formula`]s typed variants.
///
/// The [`RewritingRule`] implementation of this rule below violates
/// the _minimize_ condition by constructing
/// additional [`Formula::Not`] via wrapping the intermediate result into another [`Box`]
/// for the following _Non_ variants:
/// - [`MaterialNonImplication`][functions::MaterialNonImplication];
/// - [`ConverseNonImplication`][functions::ConverseNonImplication];
/// - [`NonDisjunction`][functions::NonDisjunction];
/// - [`NonConjunction`][functions::NonConjunction].
///
/// Anyway, it should be considered some kind of minimizing,
/// because the [`Formula`] gets more apparent and readable structure as a result.
pub struct NoDynamicConnective;

impl<T> RewritingRule<T> for NoDynamicConnective {
    fn minimize(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        if let Formula::Other(conn) = formula {
            Ok(conn.into_canonical())
        } else {
            Err(formula)
        }
    }
}
