use crate::formula::ops::Equivalent as _;

use super::{super::formula::Formula, RewritingRule};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Simplify a [`Formula`] by unwrapping
/// the negation of con(dis)-junction
/// into a dis(con)-junction of negations.
///
/// - ¬(p ∨ q) ≡ ¬p ∧ ¬q
/// - ¬(p ∧ q) ≡ ¬p ∨ ¬q
///
/// <https://en.wikipedia.org/wiki/De_Morgan's_laws>
pub struct DeMorgan;

impl<T> RewritingRule<T> for DeMorgan {
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        if let Formula::Not(f) = formula {
            match *f {
                Formula::TruthValue(_)
                | Formula::Atomic(_)
                | Formula::Not(_)
                | Formula::Xor(_, _)
                | Formula::Implies(_, _)
                | Formula::Equivalent(_, _)
                | Formula::Dynamic(_) => Err(!f),
                Formula::And(p, q) => Ok(!p | !q),
                Formula::Or(p, q) => Ok(!p & !q),
            }
        } else {
            Err(formula)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Simplify a [`Formula`] by replacing the negation
/// - of xor with equivalence (`¬(p ⊕ q) ≡ p ↔ q`)
/// - of equivalence with xor (`¬(p ↔ q) ≡ p ⊕ q`)
pub struct XorEquivNegation;

impl<T> RewritingRule<T> for XorEquivNegation {
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        if let Formula::Not(f) = formula {
            match *f {
                Formula::TruthValue(_)
                | Formula::Atomic(_)
                | Formula::Not(_)
                | Formula::And(_, _)
                | Formula::Or(_, _)
                | Formula::Implies(_, _)
                // TODO: generic negating connectives
                | Formula::Dynamic(_) => Err(!f),
                Formula::Xor(p, q) => Ok(p.equivalent(q)),
                Formula::Equivalent(p, q) => Ok(p ^ q),
            }
        } else {
            Err(formula)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Replace an exclusive disjunction in a [`Formula`]
/// with a combination of disjunctions and conjunctions:
/// `p ⊕ q ≡ (p ∨ q) ∧ (¬p ∨ ¬q)`
pub struct Xor;

impl<T: Clone> RewritingRule<T> for Xor {
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        Err(formula)
    }

    fn transform(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        if let Formula::Xor(p, q) = formula {
            Ok((p.clone() | q.clone()) & (!p | !q))
        } else {
            Err(formula)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Replace an equivalence in a [`Formula`]
/// with a combination of disjunctions and conjunctions:
/// `p ↔ q ≡ (p ∧ q) ∨ (¬p ∧ ¬q)`
pub struct Equiv;

impl<T: Clone> RewritingRule<T> for Equiv {
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        Err(formula)
    }

    fn transform(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        if let Formula::Equivalent(p, q) = formula {
            Ok((p.clone() & q.clone()) | (!p & !q))
        } else {
            Err(formula)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Replace an implication in a [`Formula`] with a disjunction:
/// `p → q ≡ ¬p ∨ q`
pub struct Implication;

impl<T> RewritingRule<T> for Implication {
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        if let Formula::Implies(f1, f2) = formula {
            Ok(!f1 | f2)
        } else {
            Err(formula)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::formula::ops::Implies as _;

    use super::*;

    #[test]
    fn conjunction() {
        let p = Formula::atom('p');
        let q = Formula::atom('q');

        // ¬(¬p ∧ q) ≡ p ∨ ¬q
        let formula = !(!p.clone() & q.clone());
        let result = DeMorgan.reduce(formula);
        assert_eq!(result.unwrap(), !!p | !q);
    }

    #[test]
    fn disjunction() {
        let p = Formula::atom('p');
        let q = Formula::atom('q');

        // ¬(¬p ∨ ¬q) ≡ p ∧ q
        let formula = !(!p.clone() | !q.clone());
        let result = DeMorgan.reduce(formula);
        assert_eq!(result.unwrap(), !!p & !!q);
    }

    #[test]
    fn equivalence() {
        let p = Formula::atom('p');
        let q = Formula::atom('q');

        // ¬(p ↔ ¬q) ≡ p ⊕ ¬q
        let formula = !(p.clone().equivalent(!q.clone()));
        let result = XorEquivNegation.reduce(formula);
        assert_eq!(result.unwrap(), p ^ !q);
    }

    #[test]
    fn xor() {
        let p = Formula::atom('p');
        let q = Formula::atom('q');

        // ¬(p ⊕ q) ≡ p ↔ q
        let formula = !(p.clone() ^ q.clone());
        let result = XorEquivNegation.reduce(formula);
        assert_eq!(result.unwrap(), p.equivalent(q));
    }

    #[test]
    fn implication() {
        let p = Formula::atom('p');
        let q = Formula::atom('q');

        // `p → ¬q ≡ ¬p ∨ ¬q`
        let formula = p.clone().implies(!q.clone());
        let result = Implication.reduce(formula);
        assert_eq!(result.unwrap(), !p | !q);
    }
}
