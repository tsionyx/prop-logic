use super::{super::formula::Formula, RewritingRule};

// TODO: compose normal forms using the combination of specific rules for every kind of NF.

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Simplify a [`Formula`] by applying the rule of distributivity
/// to conjunction where one of the operand is a disjunction:
///
/// `p ∧ (q ∨ r) ≡ (p ∧ q) ∨ (p ∧ r)`
///
/// This transformation is useful to convert a [`Formula`] into _disjunctive normal form_.
///
/// <https://en.wikipedia.org/wiki/Distributive_property#Propositional_logic>
pub struct DistributeConjunctionOverDisjunction;

impl<T: Clone> RewritingRule<T> for DistributeConjunctionOverDisjunction {
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        Err(formula)
    }

    fn transform(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        if let Formula::And(p, r) = formula {
            // (p ∨ q) ∧ r ≡ (p ∧ r) ∨ (q ∧ r)
            if let Formula::Or(p, q) = *p {
                Ok((p & r.clone()) | (q & r))
            }
            // p ∧ (q ∨ r) ≡ (p ∧ q) ∨ (p ∧ r)
            else if let Formula::Or(q, r) = *r {
                Ok((p.clone() & q) | (p & r))
            } else {
                Err(p & r)
            }
        } else {
            Err(formula)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Simplify a [`Formula`] by applying the rule of distributivity
/// to disjunction where one of the operand is a conjunction:
///
/// `p ∨ (q ∧ r) ≡ (p ∨ q) ∧ (p ∨ r)`
///
/// This transformation is useful to convert a [`Formula`] into _conjunctive normal form_.
///
/// <https://en.wikipedia.org/wiki/Distributive_property#Propositional_logic>
pub struct DistributeDisjunctionOverConjunction;

impl<T: Clone> RewritingRule<T> for DistributeDisjunctionOverConjunction {
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        Err(formula)
    }

    fn transform(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        if let Formula::Or(p, r) = formula {
            // (p ∧ q) ∨ r ≡ (p ∨ r) ∧ (q ∨ r)
            if let Formula::And(p, q) = *p {
                Ok((p | r.clone()) & (q | r))
            }
            // p ∨ (q ∧ r) ≡ (p ∨ q) ∧ (p ∨ r)
            else if let Formula::And(q, r) = *r {
                Ok((p.clone() | q) & (p | r))
            } else {
                Err(p | r)
            }
        } else {
            Err(formula)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        super::{reduce_all, RewritingRuleDebug},
        *,
    };

    #[test]
    fn simplify_to_dnf() {
        use crate::{
            connective::NonConjunction, formula::ops::Equivalent as _,
            truth_table::TruthTabled as _,
        };

        let [p, q, r] = ['p', 'q', 'r'].map(Formula::atom);

        // https://en.wikipedia.org/wiki/Disjunctive_normal_form#..._by_semantic_means
        let f = (!(p.clone() & q.clone())).equivalent(Formula::binary(
            NonConjunction,
            !r.clone(),
            p.clone() ^ q.clone(),
        ));

        let plus_distrib: Box<dyn RewritingRuleDebug<char>> =
            Box::new(DistributeConjunctionOverDisjunction);
        let f2 = reduce_all(f, true, Some(plus_distrib));

        let expected = (!p.clone() & r.clone()) | (!p & !q.clone()) | (!q & r);
        assert!(f2.is_equivalent(&expected));
        // TODO: assert_eq!(f2, expected);
    }

    #[test]
    fn simplify_to_cnf() {
        use crate::{
            connective::NonConjunction, formula::ops::Equivalent as _,
            truth_table::TruthTabled as _,
        };

        let [p, q, r] = ['p', 'q', 'r'].map(Formula::atom);

        // https://en.wikipedia.org/wiki/Conjunctive_normal_form#Conversion_by_syntactic_means
        let f = (!(p.clone() & q.clone())).equivalent(Formula::binary(
            NonConjunction,
            !r.clone(),
            p.clone() ^ q.clone(),
        ));

        let plus_distrib: Box<dyn RewritingRuleDebug<char>> =
            Box::new(DistributeDisjunctionOverConjunction);
        let f2 = reduce_all(f, true, Some(plus_distrib));

        let expected = (!p.clone() | !q.clone() | !r.clone())
            & (!p.clone() | !q.clone() | r.clone())
            & (!p.clone() | q.clone() | r.clone())
            & (p | !q | r);
        assert!(f2.is_equivalent(&expected));
        // TODO: assert_eq!(f2, expected);
    }
}
