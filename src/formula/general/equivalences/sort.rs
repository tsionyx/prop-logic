use crate::formula::ops::Implies as _;

use super::{super::formula::Formula, RewritingRule};

#[cfg_attr(not(test), allow(dead_code))]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// The rule ensures that same operands goes
/// in right-associative and consecutive order
/// using the rules of associativity and commutativity, e.g.:
/// - (p ∧ q) ∧ r ≡ p ∧ (q ∧ r)
/// - (p ∨ r) ∨ q ≡ p ∨ (q ∨ r)
///
/// Also it combines the usage of the same [variables][Formula::Atomic] together
/// to apply the [`Idempotence`][super::eq::Idempotence] or [`Negation`][super::eq::Negation] rule, e.g:
/// - p ∧ (p ∧ r) ≡ (p ∧ p) ∧ r ≡ p ∧ r
/// - p ∨ (q ∨ ¬p) [IF p < q] ≡ p ∨ (¬p v q) ≡ (q ∨ ¬q) v p ≡ p
///
/// This transformations also moves
/// the [atomic][Formula::Atomic] values (or their [negations][Formula::Not]) to occur later.
///
/// This will ease applying the following rules (like idempotent or negation rule).
pub struct SortAssociativeOperators;

/// Sort the three items from lowest to greatest.
fn sort_three<T, F: Fn(&T, &T) -> bool>(x: T, y: T, z: T, less_f: F) -> (T, T, T) {
    // swap variables?
    let x_y: bool = less_f(&y, &x);
    let x_z = less_f(&z, &x);
    let y_z = less_f(&z, &y);

    match (x_y, x_z, y_z) {
        // x <= y, x <= z, y <= z
        (false, false, false) => (x, y, z),
        // x <= y, x <= z, y > z
        (false, false, true) => (x, z, y),
        (false, true, false) => panic!("Impossible: x <= y, x > z, y <= z"),
        // x <= y, x > z, y > z
        (false, true, true) => (z, x, y),
        // x > y, x <= z, y <= z
        (true, false, false) => (y, x, z),
        (true, false, true) => panic!("Impossible: x > y, x <= z, y > z"),
        // x > y, x > z, y <= z
        (true, true, false) => (y, z, x),
        // x > y, x > z, y > z
        (true, true, true) => (z, y, x),
    }
}

macro_rules! group_consecutive {
    ($variant:ident, $p:ident, $r:ident) => {{
        use std::convert::identity;

        use crate::formula::Literal;

        use Formula::$variant as Op;

        if let Op(p, q) = *$p {
            // (p + q) + r
            if are_same_var(&p, &q) {
                // left as is: (p + q) + r
                Err(Op(Box::new(Op(p, q)), $r))
            } else if are_same_var(&p, &$r) {
                // use the commutative property
                // to combine properly:
                // (p + q) + r => (p + r) + q
                Ok(Op(Box::new(Op(p, $r)), q))
            } else if are_same_var(&q, &$r) {
                // made it left-associative:
                // (p + q) + r => p + (q + r)
                Ok(Op(p, Box::new(Op(q, $r))))
            } else {
                let vars = [*p, *q, *$r].map(Literal::try_from);
                if let [Ok(atom_p), Ok(atom_q), Ok(atom_r)] = vars {
                    // ensure original right-associativity but with ordering:
                    // (p + r) + q => (p + q) + r
                    let atoms = sort_three(atom_p, atom_q, atom_r, |x, y| x.as_var() < y.as_var());
                    let [p, q, r] = <[_; 3]>::from(atoms).map(Formula::from).map(Box::new);
                    Ok(Op(Box::new(Op(p, q)), r))
                } else {
                    let [p, q, r] = vars
                        .map(|x| x.map_or_else(identity, Formula::from))
                        .map(Box::new);

                    // provide additional right-associativity
                    // if possible:
                    if let Op(s, t) = *q {
                        // (p + (s + t)) + r => ((p + s) + t) + r
                        Ok(Op(Box::new(Op(Box::new(Op(p, s)), t)), r))
                    } else if let Op(s, t) = *r {
                        // (p + q) + (s + t) => ((p + q) + s) + t
                        Ok(Op(Box::new(Op(Box::new(Op(p, q)), s)), t))
                    } else {
                        // restore original right-associativity: (p + q) + r
                        Err(Op(Box::new(Op(p, q)), r))
                    }
                }
            }
        } else if let Op(q, r) = *$r {
            // p + (q + r)
            if are_same_var(&$p, &q) {
                // use the commutative property:
                // p + (q + r) => (p + q) + r
                Ok(Op(Box::new(Op($p, q)), r))
            } else if are_same_var(&$p, &r) {
                // use the commutative property
                // to combine properly:
                // p + (q + r) => (p + r) + q
                Ok(Op(Box::new(Op($p, r)), q))
            } else if are_same_var(&q, &r) {
                // left as is even if it is left-associative:
                // p + (q + r)
                Err(Op($p, Box::new(Op(q, r))))
            } else {
                let vars = [*$p, *q, *r].map(Literal::try_from);
                let [p, q, r] = if let [Ok(atom_p), Ok(atom_q), Ok(atom_r)] = vars {
                    // ensure right-associativity but with ordering:
                    // r + (p + q) => (p + q) + r
                    let atoms = sort_three(atom_p, atom_q, atom_r, |x, y| x.as_var() < y.as_var());
                    <[_; 3]>::from(atoms).map(Formula::from)
                } else {
                    // simply ensure right-associativity:
                    // p + (q + r) => (p + q) + r
                    vars.map(|x| x.map_or_else(identity, Formula::from))
                }
                .map(Box::new);
                Ok(Op(Box::new(Op(p, q)), r))
            }
        } else {
            let swap_operands = if let Some(f1) = $p.as_literal() {
                if let Some(f2) = $r.as_literal() {
                    f1.as_var() > f2.as_var()
                } else {
                    // Atom + Complex converted to Complex + Atom
                    true
                }
            } else {
                false
            };

            if swap_operands {
                Ok(Op($r, $p))
            } else {
                Err(Op($p, $r))
            }
        }
    }};
}

impl<T: Ord> RewritingRule<T> for SortAssociativeOperators {
    #[expect(clippy::cognitive_complexity)]
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        match formula {
            Formula::TruthValue(_) | Formula::Atomic(_) | Formula::Not(_) | Formula::Dynamic(_) => {
                Err(formula)
            }
            Formula::And(p, r) => group_consecutive!(And, p, r),
            Formula::Or(p, r) => group_consecutive!(Or, p, r),
            Formula::Xor(p, r) => group_consecutive!(Xor, p, r),
            Formula::Equivalent(p, r) => group_consecutive!(Equivalent, p, r),
            Formula::Implies(p, r) => {
                if let Formula::Implies(q, r) = *r {
                    // p → (q → r)
                    let swap_operands = if are_same_var(&p, &r) {
                        // if p and r is of the same variable, group them together
                        true
                    } else if let (Some(p), Some(q)) = (p.as_literal(), q.as_literal()) {
                        // If both p and q are literals, move the lower one (q) closer to the beginning.
                        //
                        // This rule originally should belong to the [`SortOperands`],
                        // because it relies on the commutativity but in the complex associativity-like case.
                        p.as_var() > q.as_var()
                    } else {
                        false
                    };
                    if swap_operands {
                        // consider the commutativity of antecedents:
                        // p → (q → r) ≡ q → (p → r)
                        Ok(q.implies(p.implies(r)))
                    } else {
                        // restore original
                        Err(p.implies(q.implies(r)))
                    }
                } else {
                    Err(p.implies(r))
                }
            }
        }
    }
}

fn are_same_var<T: PartialEq>(p: &Formula<T>, q: &Formula<T>) -> bool {
    if let (Some(p), Some(q)) = (p.as_literal(), q.as_literal()) {
        p.as_var() == q.as_var()
    } else {
        false
    }
}
