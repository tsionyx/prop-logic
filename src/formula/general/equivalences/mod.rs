//! The rules to convert a [`Formula`][super::formula::Formula]
//! to the equivalent one by using common equivalence laws.
//!
//! <https://en.wikipedia.org/wiki/Logical_equivalence>

// TODO: more rules:
// - De Morgan's laws:
//
// Conversion to specific form:
// - Distributive laws:
//   - Disjunction over Conjunction and vice versa;
//   - Conjunction over Disjunction;
//   - Implication over Conjunction and Disjunction;
//   - Left-distributivity of Implication;
// - Implication:
//   - to Disjunction and Negation; (.compose)
//   - Non-Implication to Conjunction and Negation (.compose for MaterialNonImplication);
//   - Conjunction of same consequents is equivalent to Disjunction of antecedents;
//   - Disjunction of same consequents is equivalent to Conjunction of antecedents;
//   - duality with ConverseImplication;
// - Equivalence:
//   - Conjunction of Implication and ConverseImplication;
//   - negate both operands;
mod absorption;
mod canonical;
mod constant;
mod eq;
mod neg;
mod sort;
mod traits;

pub use traits::RewritingRule;
// TODO: compose normal forms using the combination of specific rules for every kind of NF.

/// Recommended order of the rules to apply in order to convert
/// a [`Formula`][super::formula::Formula]
/// into equivalent one while reducing it to the maximum.
pub fn rules_in_order<T>() -> Vec<Box<dyn RewritingRule<T>>>
where
    T: Ord,
{
    vec![
        Box::new(constant::EliminateConstants),
        Box::new(canonical::NoDynamicConnective), // idempotent (only a single run is necessary)
        Box::new(neg::DoubleNegation),
        Box::new(sort::SortAssociativeOperators),
        Box::new(sort::SortOperands),
        Box::new(eq::Idempotence),
        Box::new(eq::Negation),
        Box::new(absorption::Absorption),
    ]
}
