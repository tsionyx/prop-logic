//! Simplified representation of [`Formula`][super::super::Formula]
//! in the so-called _Negation normal form_.
//!
//! This representation consists of arbitrary set
//! of literals and sub-expressions connected only with
//! the [AND operation][crate::connective::Conjunction] and
//! the [OR operation][crate::connective::Disjunction]
//! without any additional constraints on the structure.
//!
//! This form can also be viewed as a special case of [`Formula`][super::super::Formula]
//! in the ([functionally complete][crate::connective::is_complete]) set of:
//! - [NOT][crate::connective::Negation];
//! - [AND][crate::connective::Conjunction];
//! - [OR][crate::connective::Disjunction].
//!
//! <https://en.wikipedia.org/wiki/Negation_normal_form>
use std::fmt::{self, Debug, Display};

use crate::{
    connective::{series, Conjunction, Connective, Disjunction, Evaluable},
    utils::vec::UnsortedVec,
};

use super::{
    super::{equivalences::RewritingRuleDebug, Formula, Signed},
    error::Error,
    NormalForm as NormalFormTrait,
};

type Literal<T> = Signed<T>;

#[derive(Debug, PartialEq, Eq, Clone)]
/// Arbitrary set of literals and sub-expressions connected with
/// the [AND operation] or [OR operation].
pub enum NormalForm<T> {
    /// The simplest type of [`NormalForm`] with no deeper propositional structure.
    Literal(Literal<T>),

    /// A number of conjunctions of the sub-formulae.
    And(UnsortedVec<Self>),

    /// A number of disunctions of the sub-formulae.
    Or(UnsortedVec<Self>),
}

impl<T> From<NormalForm<T>> for Formula<T> {
    fn from(value: NormalForm<T>) -> Self {
        match value {
            NormalForm::Literal(lit) => lit.into(),
            NormalForm::And(c) => series(&Conjunction, c.into_iter().map(Self::from)),
            NormalForm::Or(d) => series(&Disjunction, d.into_iter().map(Self::from)),
        }
    }
}

impl<T> Evaluable for NormalForm<T> {
    type Partial = Self;

    fn terminal(value: bool) -> Self {
        let empty = Vec::new().into();
        if value {
            Self::And(empty)
        } else {
            Self::Or(empty)
        }
    }

    fn is_tautology(&self) -> bool {
        matches!(self, Self::And(conjuncts) if conjuncts.as_ref().is_empty())
    }

    fn is_contradiction(&self) -> bool {
        matches!(self, Self::Or(disjuncts) if disjuncts.as_ref().is_empty())
    }

    fn partial(val: Self) -> Self {
        val
    }

    fn into_terminal(self) -> Result<bool, Self> {
        match self {
            Self::And(conjuncts) if conjuncts.as_ref().is_empty() => Ok(true),
            Self::Or(disjuncts) if disjuncts.as_ref().is_empty() => Ok(false),
            other => Err(other),
        }
    }
}

impl<T> NormalForm<T> {
    const fn as_literal(&self) -> Option<&Literal<T>> {
        if let Self::Literal(l) = self {
            Some(l)
        } else {
            None
        }
    }
}

impl<T: PartialEq> NormalForm<T> {
    /// Create a [`NormalForm`] from a number of conjuncts.
    pub fn and(conjuncts: Vec<Self>) -> Self {
        let mut visited = Vec::with_capacity(conjuncts.len());

        for conjunct in conjuncts {
            if conjunct.is_tautology() {
                // just ignore the current
                continue;
            }

            if conjunct.is_contradiction() {
                // the whole sequence is `⊥`
                return Self::contradiction();
            }

            if let Self::Literal(lit) = &conjunct {
                // detect repeating variables
                let found_negated = visited
                    .iter()
                    .filter_map(Self::as_literal)
                    .any(|l| l.by_ref() == !lit.by_ref());

                // if have `p` and `¬p`, then we can reduce the whole sequence to `⊥`
                if found_negated {
                    return Self::contradiction();
                }

                let found_repeating = visited
                    .iter()
                    .filter_map(Self::as_literal)
                    .any(|l| l == lit);
                if found_repeating {
                    continue;
                }
            }
            visited.push(conjunct);
        }

        Self::And(visited.into_iter().collect())
    }

    /// Create a [`NormalForm`] from a number of disjuncts.
    pub fn or(disjuncts: Vec<Self>) -> Self {
        let mut visited = Vec::with_capacity(disjuncts.len());

        for disjunct in disjuncts {
            if disjunct.is_tautology() {
                // the whole sequence is `⊤`
                return Self::tautology();
            }

            if disjunct.is_contradiction() {
                // just ignore the current
                continue;
            }

            if let Self::Literal(lit) = &disjunct {
                // detect repeating variables
                let found_negated = visited
                    .iter()
                    .filter_map(Self::as_literal)
                    .any(|l| l.by_ref() == !lit.by_ref());

                // if have `p` and `¬p`, then we can reduce the whole sequence to `⊥`
                if found_negated {
                    return Self::tautology();
                }

                let found_repeating = visited
                    .iter()
                    .filter_map(Self::as_literal)
                    .any(|l| l == lit);
                if found_repeating {
                    continue;
                }
            }
            visited.push(disjunct);
        }

        Self::Or(visited.into_iter().collect())
    }
}

impl<T> NormalForm<T>
where
    T: PartialEq,
{
    fn convert_prepared(formula: Formula<T>) -> Result<Self, Error> {
        use std::iter::once;

        match formula {
            Formula::TruthValue(t) => Ok(Self::terminal(t)),
            Formula::Atomic(x) => Ok(Self::Literal(Literal::Pos(x))),
            Formula::Not(f) => {
                if let Formula::Atomic(x) = *f {
                    Ok(Self::Literal(Literal::Neg(x)))
                } else {
                    Err(Error::UnexpectedNot {
                        normal_form: std::any::type_name::<Self>(),
                        connective: f.get_connective().map(|_| ()),
                    })
                }
            }
            Formula::And(f1, f2) => {
                let nf1 = Self::convert_prepared(*f1)?;
                let nf2 = Self::convert_prepared(*f2)?;

                let res = match (nf1, nf2) {
                    (Self::And(conjunct1), Self::And(conjunct2)) => {
                        conjunct1.into_iter().chain(conjunct2).collect()
                    }
                    (Self::And(conjunct1), other) => {
                        conjunct1.into_iter().chain(Some(other)).collect()
                    }
                    (other, Self::And(conjunct2)) => once(other).chain(conjunct2).collect(),
                    (other1, other2) => once(other1).chain(Some(other2)).collect(),
                };

                Ok(Self::and(res))
            }
            Formula::Or(f1, f2) => {
                let nf1 = Self::convert_prepared(*f1)?;
                let nf2 = Self::convert_prepared(*f2)?;

                let res = match (nf1, nf2) {
                    (Self::Or(disjunct1), Self::Or(disjunct2)) => {
                        disjunct1.into_iter().chain(disjunct2).collect()
                    }
                    (Self::Or(disjunct1), other) => {
                        disjunct1.into_iter().chain(Some(other)).collect()
                    }
                    (other, Self::Or(disjunct2)) => once(other).chain(disjunct2).collect(),
                    (other1, other2) => once(other1).chain(Some(other2)).collect(),
                };

                Ok(Self::or(res))
            }
            Formula::Xor(_, _)
            | Formula::Implies(_, _)
            | Formula::Equivalent(_, _)
            | Formula::Dynamic(_) => Err(Error::InvalidConnective {
                normal_form: std::any::type_name::<Self>(),
                connective: formula.get_connective().map(|_| ()),
            }),
        }
    }
}

impl<T> TryFrom<Formula<T>> for NormalForm<T>
where
    T: PartialEq + Clone + 'static,
{
    type Error = Error;

    fn try_from(formula: Formula<T>) -> Result<Self, Self::Error> {
        let formula = Self::prepare(formula);
        Self::convert_prepared(formula)
    }
}

impl<T> NormalFormTrait<T> for NormalForm<T>
where
    T: PartialEq + Clone + 'static,
{
    fn rules<V: PartialEq + Clone>() -> Vec<Box<dyn RewritingRuleDebug<V>>> {
        #![allow(clippy::wildcard_imports)]
        use super::super::equivalences::*;

        vec![
            // no `Formula::Truth` aside from top-level
            Box::new(constant::EliminateConstants),
            // no `Formula::Dynamic`
            Box::new(canonical::NoDynamicConnective),
            // no `Formula::Implies`
            Box::new(elimination::Implication),
            // no `Formula::Not(Formula::Xor(...))` and `Formula::Not(Formula::Equivalent(...))`
            Box::new(elimination::XorEquivNegation),
            // no `Formula::Xor`
            Box::new(elimination::Xor),
            // no `Formula::Equivalent`
            Box::new(elimination::Equiv),
            // no `Formula::Not(Formula::And(...))` and `Formula::Not(Formula::Or(...))`
            Box::new(elimination::DeMorgan),
            // no `Formula::Not(Formula::Not(...))`
            Box::new(neg::DoubleNegation),
            // At this point, the formula should be in NNF form already.
            // The following rules can be useful to optimize early,
            // but no guarantees are provided as the rules
            // are dependent on the order of variables
            // in associative and commutative operations.
            Box::new(eq::Idempotence),
            Box::new(eq::Negation),
            Box::new(absorption::Absorption),
            Box::new(absorption::AbsorptionWithNeg),
        ]
    }
}

impl<T: Display> Display for NormalForm<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Literal(lit) => Display::fmt(&lit, f),
            Self::And(conjuncts) => {
                write!(f, "{}(", Conjunction.notation())?;
                let mut first = true;
                for conjunct in conjuncts.as_ref() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    Display::fmt(conjunct, f)?;
                    first = false;
                }
                write!(f, ")")
            }
            Self::Or(disjuncts) => {
                write!(f, "{}(", Disjunction.notation())?;
                let mut first = true;
                for conjunct in disjuncts.as_ref() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    Display::fmt(conjunct, f)?;
                    first = false;
                }
                write!(f, ")")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn conjunction_consts() {
        let f: Formula<char> = Formula::truth(true) & Formula::truth(true);
        let nf = NormalForm::convert_prepared(f).unwrap();
        assert_eq!(nf, NormalForm::tautology());

        let f: Formula<char> = Formula::truth(true) & Formula::truth(false);
        let nf = NormalForm::convert_prepared(f).unwrap();
        assert_eq!(nf, NormalForm::contradiction());

        let f: Formula<char> = Formula::truth(false) & Formula::truth(true);
        let nf = NormalForm::convert_prepared(f).unwrap();
        assert_eq!(nf, NormalForm::contradiction());

        let f: Formula<char> = Formula::truth(false) & Formula::truth(false);
        let nf = NormalForm::convert_prepared(f).unwrap();
        assert_eq!(nf, NormalForm::contradiction());
    }

    #[test]
    fn conjunction_with_truth() {
        let f = Formula::truth(true) & 'a';
        let nf = NormalForm::convert_prepared(f).unwrap();

        let lit = NormalForm::Literal('a'.into());
        assert_eq!(nf, NormalForm::And(vec![lit].into()));
    }

    #[test]
    fn conjunction_with_false() {
        let f = Formula::truth(false) & 'a';
        let nf = NormalForm::convert_prepared(f).unwrap();

        assert_eq!(nf, NormalForm::contradiction());
    }

    #[test]
    fn constant_truth() {
        let nf = NormalForm::tautology();
        assert!(nf.is_tautology());
        assert!(!nf.is_contradiction());
        let f: Formula<char> = nf.clone().into();
        assert!(f.is_tautology());
        let nf2 = NormalForm::try_from(f).unwrap();
        assert_eq!(nf, nf2);
        assert!(nf.into_terminal().unwrap());
    }

    #[test]
    fn disjunction_consts() {
        let f: Formula<char> = Formula::truth(true) | Formula::truth(true);
        let nf = NormalForm::convert_prepared(f).unwrap();
        assert_eq!(nf, NormalForm::tautology());

        let f: Formula<char> = Formula::truth(true) | Formula::truth(false);
        let nf = NormalForm::convert_prepared(f).unwrap();
        assert_eq!(nf, NormalForm::tautology());

        let f: Formula<char> = Formula::truth(false) | Formula::truth(true);
        let nf = NormalForm::convert_prepared(f).unwrap();
        assert_eq!(nf, NormalForm::tautology());

        let f: Formula<char> = Formula::truth(false) | Formula::truth(false);
        let nf = NormalForm::convert_prepared(f).unwrap();
        assert_eq!(nf, NormalForm::contradiction());
    }

    #[test]
    fn disjunction_with_truth() {
        let f = Formula::truth(true) | 'a';
        let nf = NormalForm::convert_prepared(f).unwrap();

        assert_eq!(nf, NormalForm::tautology());
    }

    #[test]
    fn disjunction_with_false() {
        let f = Formula::truth(false) | 'a';
        let nf = NormalForm::convert_prepared(f).unwrap();

        let lit = NormalForm::Literal('a'.into());
        assert_eq!(nf, NormalForm::Or(vec![lit].into()));
    }

    #[test]
    fn constant_false() {
        let nf = NormalForm::contradiction();
        assert!(nf.is_contradiction());
        assert!(!nf.is_tautology());
        let f: Formula<char> = nf.clone().into();
        assert!(f.is_contradiction());
        let nf2 = NormalForm::try_from(f).unwrap();
        assert_eq!(nf, nf2);
        assert!(!nf.into_terminal().unwrap());
    }
}

#[cfg(all(test, feature = "arbitrary"))]
mod prop_test {
    use proptest::prelude::*;

    use crate::{
        formula::{Formula, FormulaParameters},
        truth_table::TruthTabled as _,
    };

    use super::*;

    fn params() -> FormulaParameters<char> {
        FormulaParameters {
            variables: vec!['a', 'b', 'c', 'd'],
            leaf_var_weight: Some(10),
            use_dynamic: true,
            ..FormulaParameters::default()
        }
    }

    proptest! {
        // https://proptest-rs.github.io/proptest/proptest/tutorial/config.html
        #![proptest_config(ProptestConfig::with_cases(100))]

        #[test]
        fn roundtrip_conversion_to_nf(f in Formula::arbitrary_with(params())) {
            eprintln!("1. Original: {f:#}:\n{}", f.get_truth_table());
            let reduced = NormalForm::<char>::prepare(f.clone());
            eprintln!("2. Reduced out of NF: {reduced:#}: \n{}", reduced.get_truth_table());
            let nf = NormalForm::try_from(f.clone()).unwrap();
            eprintln!("4. NF: {nf}");
            let f2: Formula<char> = nf.into();
            eprintln!("5. Result: {f2:#}:\n{}", f2.get_truth_table());
            assert!(f.is_equivalent(&f2));
        }

    }
}
