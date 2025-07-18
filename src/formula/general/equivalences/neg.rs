use super::{super::formula::Formula, RewritingRule};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Simplify the unary relation of two consecutive Negation.
pub struct DoubleNegation;

impl<T> RewritingRule<T> for DoubleNegation {
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        if let Formula::Not(f) = formula {
            if let Formula::Not(f) = *f {
                Ok(*f)
            } else {
                Err(!f)
            }
        } else {
            Err(formula)
        }
    }
}
