use super::{
    super::formula::Formula,
    eq::{are_each_other_negation, are_same_var},
    RewritingRule,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Simplify the binary relation when there are
/// two **different** operations involving the same variable:
///
/// - p ∧ (p ∨ q) ≡ p
/// - p ∨ (p ∧ q) ≡ p
/// - p → (p ∧ q) ≡ p → q
pub struct Absorption;

impl<T: PartialEq> RewritingRule<T> for Absorption {
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        match formula {
            Formula::TruthValue(_)
            | Formula::Atomic(_)
            | Formula::Not(_)
            | Formula::Xor(_, _)
            | Formula::Equivalent(_, _)
            | Formula::Dynamic(_) => Err(formula),
            Formula::And(p, r) => {
                if let Formula::Or(q, r) = *r {
                    // p ∧ (p ∨ r) ≡ p
                    // p ∧ (q ∨ p) ≡ p
                    if are_same_var(&p, &q) || are_same_var(&p, &r) {
                        Ok(*p)
                    } else {
                        Err(p & (q | r))
                    }
                } else if let Formula::Or(p, q) = *p {
                    // (p ∨ q) ∧ p ≡ p
                    // (p ∨ q) ∧ q ≡ q
                    if are_same_var(&p, &r) || are_same_var(&q, &r) {
                        Ok(*r)
                    } else {
                        Err((p | q) & r)
                    }
                } else {
                    Err(p & r)
                }
            }
            Formula::Or(p, r) => {
                if let Formula::And(q, r) = *r {
                    // p ∨ (p ∧ r) ≡ p
                    // p ∨ (q ∧ p) ≡ p
                    if are_same_var(&p, &q) || are_same_var(&p, &r) {
                        Ok(*p)
                    } else {
                        Err(p | (q & r))
                    }
                } else if let Formula::And(p, q) = *p {
                    // (p ∧ q) ∨ p ≡ p
                    // (p ∧ q) ∨ q ≡ q
                    if are_same_var(&p, &r) || are_same_var(&q, &r) {
                        Ok(*r)
                    } else {
                        Err((p & q) | r)
                    }
                } else {
                    Err(p | r)
                }
            }
            Formula::Implies(p, r) => {
                use crate::formula::ops::Implies as _;

                // https://en.wikipedia.org/wiki/Absorption_(logic)
                if let Formula::And(q, r) = *r {
                    if are_same_var(&p, &q) {
                        // p → (p ∧ q) ≡ p → q
                        Ok(p.implies(r))
                    } else if are_same_var(&p, &r) {
                        // p → (q ∧ p) ≡ p → q
                        Ok(p.implies(q))
                    } else {
                        Err(p.implies(q & r))
                    }
                } else {
                    Err(p.implies(r))
                }
            }
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Simplify the binary relation when there are
/// two **different** operations involving the same variable with **different** sign:
///
/// - p ∧ (¬p ∨ q) ≡ p ∧ q
/// - p ∨ (¬p ∧ q) ≡ p ∨ q
pub struct AbsorptionWithNeg;

impl<T: PartialEq> RewritingRule<T> for AbsorptionWithNeg {
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        match formula {
            Formula::TruthValue(_)
            | Formula::Atomic(_)
            | Formula::Not(_)
            | Formula::Implies(_, _)
            | Formula::Xor(_, _)
            | Formula::Equivalent(_, _)
            | Formula::Dynamic(_) => Err(formula),
            Formula::And(p, r) => {
                if let Formula::Or(q, r) = *r {
                    // p ∧ (¬p ∨ r) ≡ p ∧ r
                    if are_each_other_negation(&p, &q) {
                        Ok(p & r)
                    // p ∧ (q ∨ ¬p) ≡ p ∧ q
                    } else if are_each_other_negation(&p, &r) {
                        Ok(p & q)
                    } else {
                        Err(p & (q | r))
                    }
                } else if let Formula::Or(p, q) = *p {
                    // (¬r ∨ q) ∧ r ≡ q ∧ r
                    if are_each_other_negation(&p, &r) {
                        Ok(q & r)
                    // (p ∨ ¬r) ∧ r ≡ p ∧ r
                    } else if are_each_other_negation(&q, &r) {
                        Ok(p & r)
                    } else {
                        Err((p | q) & r)
                    }
                } else {
                    Err(p & r)
                }
            }
            Formula::Or(p, r) => {
                if let Formula::And(q, r) = *r {
                    // p ∨ (¬p ∧ r) ≡ p ∨ r
                    if are_each_other_negation(&p, &q) {
                        Ok(p | r)
                    // p ∨ (q ∧ ¬p) ≡ p ∨ q
                    } else if are_each_other_negation(&p, &r) {
                        Ok(p | q)
                    } else {
                        Err(p | (q & r))
                    }
                } else if let Formula::And(p, q) = *p {
                    // (¬r ∧ q) ∨ r ≡ q ∨ r
                    if are_each_other_negation(&p, &r) {
                        Ok(q | r)
                    // (p ∧ ¬r) ∨ r ≡ p ∨ r
                    } else if are_each_other_negation(&q, &r) {
                        Ok(p | r)
                    } else {
                        Err((p & q) | r)
                    }
                } else {
                    Err(p | r)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::formula::{ops::Implies as _, Formula};

    #[test]
    fn conjunction() {
        let p = Formula::atom('p');
        let q = Formula::atom('q');

        // p ∧ (p ∨ q) ≡ p
        let formula = p.clone() & (p.clone() | q.clone());
        let result = Absorption.reduce(formula);
        assert_eq!(result.as_ref(), Ok(&p));

        let formula = p.clone() & (q.clone() | p.clone());
        let result = Absorption.reduce(formula);
        assert_eq!(result.as_ref(), Ok(&p));

        // (p ∨ q) ∧ p ≡ p
        let formula = (p.clone() | q.clone()) & p.clone();
        let result = Absorption.reduce(formula);
        assert_eq!(result.as_ref(), Ok(&p));

        let formula = (q | p.clone()) & p.clone();
        let result = Absorption.reduce(formula);
        assert_eq!(result.as_ref(), Ok(&p));
    }

    #[test]
    fn disjunction() {
        let p = Formula::atom('p');
        let q = Formula::atom('q');

        // p ∨ (p ∧ q) ≡ p
        let formula = p.clone() | (p.clone() & q.clone());
        let result = Absorption.reduce(formula);
        assert_eq!(result.as_ref(), Ok(&p));

        let formula = p.clone() | (q.clone() & p.clone());
        let result = Absorption.reduce(formula);
        assert_eq!(result.as_ref(), Ok(&p));

        // (p ∧ q) ∨ p ≡ p
        let formula = (p.clone() & q.clone()) | p.clone();
        let result = Absorption.reduce(formula);
        assert_eq!(result.as_ref(), Ok(&p));

        let formula = (q & p.clone()) | p.clone();
        let result = Absorption.reduce(formula);
        assert_eq!(result.as_ref(), Ok(&p));
    }

    #[test]
    fn implication() {
        let p = Formula::atom('p');
        let q = Formula::atom('q');
        let p_implies_q = Formula::implies(p.clone(), q.clone());

        // p → (p ∧ q) ≡ p → q
        let formula = p.clone().implies(p.clone() & q.clone());
        let result = Absorption.reduce(formula);
        assert_eq!(result.as_ref(), Ok(&p_implies_q));

        // p → (q ∧ p) ≡ p → q
        let formula = p.clone().implies(q & p);
        let result = Absorption.reduce(formula);
        assert_eq!(result.as_ref(), Ok(&p_implies_q));
    }
}
