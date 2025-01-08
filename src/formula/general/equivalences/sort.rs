use crate::connective::Commutativity as _;

use super::{
    super::{
        super::DynConnective,
        formula::{AnyConnective, Formula},
    },
    RewritingRule,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// The rule ensures that same operands goes in left associative order
/// using the rules of associativity, e.g.:
/// - (p ∧ q) ∧ r ≡ p ∧ (q ∧ r)
/// - (p ∨ q) ∨ r ≡ p ∨ (q ∨ r)
///
/// Also it combines the same [`Atomic`][Formula::Atomic] values together
/// to apply the [`Idempotence`][super::eq::Idempotence] or [`Negation`][super::eq::Negation] rule, e.g:
/// - p ∧ (p ∧ r) ≡ (p ∧ p) ∧ r)
/// - q ∨ (p ∨ ¬q) [IF p > q, BY `SortOperands`]≡ q ∨ (¬q v p) [BY `SortAssociativeOperators`]≡ (q ∨ ¬q) v p
pub struct SortAssociativeOperators;

macro_rules! group_consecutive {
    ($variant:ident, $p:ident, $r:ident) => {{
        if let Formula::$variant(p, q) = *$p {
            // (p + q) + r
            let group_same_literals =
                if let (Some(p), Some(q)) = (p.as_directed_atom(), q.as_directed_atom()) {
                    p.as_ref() == q.as_ref()
                } else {
                    false
                };

            if group_same_literals {
                // (p + q) + r  // restore original
                Err(Formula::$variant(Box::new(Formula::$variant(p, q)), $r))
            } else {
                // (p + q) + r => p + (q + r)  // ensure left-associative
                Ok(Formula::$variant(p, Box::new(Formula::$variant(q, $r))))
            }
        } else if let Formula::$variant(q, r) = *$r {
            // p + (q + r)
            let group_same_literals =
                if let (Some(p), Some(q)) = ($p.as_directed_atom(), q.as_directed_atom()) {
                    p.as_ref() == q.as_ref()
                } else {
                    false
                };

            if group_same_literals {
                // p + (q + r) => (p + q) + r  // switch even if it becomes right-associative
                Ok(Formula::$variant(Box::new(Formula::$variant($p, q)), r))
            } else {
                // (p + q) + r  // restore original left-associative
                Err(Formula::$variant($p, Box::new(Formula::$variant(q, r))))
            }
        } else {
            Err(Formula::$variant($p, $r))
        }
    }};
}

impl<T: Ord> RewritingRule<T> for SortAssociativeOperators {
    #[allow(clippy::cognitive_complexity)]
    fn minimize(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        match formula {
            Formula::TruthValue(_) | Formula::Atomic(_) | Formula::Not(_) | Formula::Other(_) => {
                Err(formula)
            }
            Formula::And(p, r) => group_consecutive!(And, p, r),
            Formula::Or(p, r) => group_consecutive!(Or, p, r),
            Formula::Xor(p, r) => group_consecutive!(Xor, p, r),
            Formula::Implies(p, r) => {
                if let Formula::Implies(q, r) = *r {
                    // p → (q → r)
                    let swap_operands = if let (Some(p), Some(r)) =
                        (p.as_directed_atom(), r.as_directed_atom())
                    {
                        // if p and r is of the same variable, group them together
                        p.as_ref() == r.as_ref()
                    } else if let (Some(p), Some(q)) = (p.as_directed_atom(), q.as_directed_atom())
                    {
                        // If both p and q are literals, move the lower one (q) closer to the beginning.
                        //
                        // This rule originally should belong to the [`SortOperands`],
                        // because it relies on the commutativity but in the complex associativity-like case.
                        p.as_ref() > q.as_ref()
                    } else {
                        false
                    };
                    if swap_operands {
                        // consider the commutativity of antecedents:
                        // p → (q → r) ≡ q → (p → r)
                        Ok(Formula::Implies(q, Box::new(Formula::Implies(p, r))))
                    } else {
                        // restore original
                        Err(Formula::Implies(p, Box::new(Formula::Implies(q, r))))
                    }
                } else {
                    Err(Formula::Implies(p, r))
                }
            }
            Formula::Equivalent(p, r) => group_consecutive!(Equivalent, p, r),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Ensure that operands (or their operands) goes in consecutive order
/// using the rules of commutativity.
///
/// Also move the [`Atomic`][Formula::Atomic] values to occur earlier.
///
/// This will ease applying the following rules (like idempotent or negation rule).
pub struct SortOperands;

impl<T: Ord> RewritingRule<T> for SortOperands {
    fn minimize(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        let conn = formula.into();
        if let AnyConnective::Binary(DynConnective {
            connective,
            operands: [f1, f2],
        }) = &conn
        {
            // detect And, Or, Xor, Eq
            #[allow(clippy::option_if_let_else)]
            let swap_operands = if connective.is_commutative() {
                if let Some(q) = f2.as_directed_atom() {
                    if let Some(p) = f1.as_directed_atom() {
                        p.as_ref() > q.as_ref()
                    } else {
                        // Complex & Atom converted to Atom & Complex
                        true
                    }
                } else {
                    false
                }
            } else {
                false
            };

            if swap_operands {
                Ok(conn.swap_operands().into_canonical())
            } else {
                Err(conn.into_canonical())
            }
        } else {
            Err(conn.into_canonical())
        }
    }
}

// TODO: tests
