use super::{super::formula::Formula, eq::are_same_atom, RewritingRule};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Simplify the binary relation when there are
/// two **different** operations involving the same atom:
///
/// - p ∧ (p ∨ q) ≡ p
/// - p ∨ (p ∧ q) ≡ p
/// - p → (p ∧ q) ≡ p → q
pub struct Absorption;

impl<T: PartialEq> RewritingRule<T> for Absorption {
    fn minimize(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        match formula {
            Formula::TruthValue(_)
            | Formula::Atomic(_)
            | Formula::Not(_)
            | Formula::Xor(_, _)
            | Formula::Equivalent(_, _)
            | Formula::Other(_) => Err(formula),
            Formula::And(p, r) => {
                if let Formula::Or(q, r) = *r {
                    // p ∧ (p ∨ r) ≡ p
                    // p ∧ (q ∨ p) ≡ p
                    if are_same_atom(&p, &q) || are_same_atom(&p, &r) {
                        Ok(*p)
                    } else {
                        Err(Formula::And(p, Box::new(Formula::Or(q, r))))
                    }
                } else if let Formula::Or(p, q) = *p {
                    // (p ∨ q) ∧ p ≡ p
                    // (p ∨ q) ∧ q ≡ q
                    if are_same_atom(&p, &r) || are_same_atom(&q, &r) {
                        Ok(*r)
                    } else {
                        Err(Formula::And(Box::new(Formula::Or(p, q)), r))
                    }
                } else {
                    Err(Formula::And(p, r))
                }
            }
            Formula::Or(p, r) => {
                if let Formula::And(q, r) = *r {
                    // p ∨ (p ∧ r) ≡ p
                    // p ∨ (q ∧ p) ≡ p
                    if are_same_atom(&p, &q) || are_same_atom(&p, &r) {
                        Ok(*p)
                    } else {
                        Err(Formula::Or(p, Box::new(Formula::And(q, r))))
                    }
                } else if let Formula::And(p, q) = *p {
                    // (p ∧ q) ∨ p ≡ p
                    // (p ∧ q) ∨ q ≡ q
                    if are_same_atom(&p, &r) || are_same_atom(&q, &r) {
                        Ok(*r)
                    } else {
                        Err(Formula::Or(Box::new(Formula::And(p, q)), r))
                    }
                } else {
                    Err(Formula::And(p, r))
                }
            }
            Formula::Implies(p, r) => {
                // https://en.wikipedia.org/wiki/Absorption_(logic)
                if let Formula::And(q, r) = *r {
                    if are_same_atom(&p, &q) {
                        // p → (p ∧ q) ≡ p → q
                        Ok(Formula::Implies(p, r))
                    } else if are_same_atom(&p, &r) {
                        // p → (q ∧ p) ≡ p → q
                        Ok(Formula::Implies(p, q))
                    } else {
                        Err(Formula::Implies(p, Box::new(Formula::And(q, r))))
                    }
                } else {
                    Err(Formula::Implies(p, r))
                }
            }
        }
    }
}

// TODO: tests
