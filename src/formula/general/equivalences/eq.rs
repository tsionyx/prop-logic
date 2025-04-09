use crate::connective::Evaluable as _;

use super::{super::formula::Formula, RewritingRule};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Simplify the binary relation when its operands are the same.
pub struct Idempotence;

impl<T: PartialEq> RewritingRule<T> for Idempotence {
    fn minimize(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        match formula {
            Formula::TruthValue(_) | Formula::Atomic(_) | Formula::Not(_) | Formula::Other(_) => {
                Err(formula)
            }
            Formula::And(f1, f2) => {
                if are_same_atom(&f1, &f2) {
                    Ok(*f1)
                } else {
                    Err(Formula::And(f1, f2))
                }
            }
            Formula::Or(f1, f2) => {
                if are_same_atom(&f1, &f2) {
                    Ok(*f1)
                } else {
                    Err(Formula::Or(f1, f2))
                }
            }
            Formula::Xor(f1, f2) => {
                if are_same_atom(&f1, &f2) {
                    Ok(Formula::contradiction())
                } else {
                    Err(Formula::Xor(f1, f2))
                }
            }
            Formula::Implies(f1, f2) => {
                if are_same_atom(&f1, &f2) {
                    Ok(Formula::tautology())
                } else {
                    Err(Formula::Implies(f1, f2))
                }
            }
            Formula::Equivalent(f1, f2) => {
                if are_same_atom(&f1, &f2) {
                    Ok(Formula::tautology())
                } else {
                    Err(Formula::Equivalent(f1, f2))
                }
            }
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Simplify the binary relation when its operands are the negation of each other.
pub struct Negation;

impl<T: PartialEq> RewritingRule<T> for Negation {
    fn minimize(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        match formula {
            Formula::TruthValue(_) | Formula::Atomic(_) | Formula::Not(_) | Formula::Other(_) => {
                Err(formula)
            }
            Formula::And(f1, f2) => {
                if are_each_other_negation(&f1, &f2) {
                    Ok(Formula::contradiction())
                } else {
                    Err(Formula::And(f1, f2))
                }
            }
            Formula::Or(f1, f2) => {
                if are_each_other_negation(&f1, &f2) {
                    Ok(Formula::tautology())
                } else {
                    Err(Formula::Or(f1, f2))
                }
            }
            Formula::Xor(f1, f2) => {
                if are_each_other_negation(&f1, &f2) {
                    Ok(Formula::tautology())
                } else {
                    Err(Formula::Xor(f1, f2))
                }
            }
            Formula::Implies(f1, f2) => {
                if are_each_other_negation(&f1, &f2) {
                    Ok(Formula::Not(f1))
                } else {
                    Err(Formula::Implies(f1, f2))
                }
            }
            Formula::Equivalent(f1, f2) => {
                if are_each_other_negation(&f1, &f2) {
                    Ok(Formula::contradiction())
                } else {
                    Err(Formula::Equivalent(f1, f2))
                }
            }
        }
    }
}

pub(super) fn are_same_atom<T: PartialEq>(p: &Formula<T>, q: &Formula<T>) -> bool {
    if let (Some(p), Some(q)) = (p.as_directed_atom(), q.as_directed_atom()) {
        p == q
    } else {
        false
    }
}

fn are_each_other_negation<T: PartialEq>(p: &Formula<T>, q: &Formula<T>) -> bool {
    if let (Some(p), Some(q)) = (p.as_directed_atom(), q.as_directed_atom()) {
        p == !q
    } else {
        false
    }
}

// TODO: tests
