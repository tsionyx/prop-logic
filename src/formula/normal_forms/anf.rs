//! Optimized representation of [`Formula`][super::super::Formula]
//! in the so-called _Algebraic normal form_
//! aka _Zhegalkin normal form_.
//!
//! This representation consists of one or more [conjunctive terms][super::Conjunct] connected
//! with the [XOR operation][crate::connective::ExclusiveDisjunction]
//! where every term can be only:
//! - a constant [Truth][crate::connective::Truth];
//! - or a series of [Conjunction][crate::connective::Conjunction]
//!   of variables in **non-negated** form.
//!
//! This form can also be viewed as a special case of [`Formula`][super::super::Formula]
//! in the ([functionally complete][crate::connective::is_basis]) basis of:
//! - [1][crate::connective::Truth];
//! - [AND][crate::connective::Conjunction];
//! - [XOR][crate::connective::ExclusiveDisjunction].
//!
//! <https://en.wikipedia.org/wiki/Algebraic_normal_form>
//! <https://en.wikipedia.org/wiki/Zhegalkin_polynomial>

use crate::{
    connective::{Evaluable, ExclusiveDisjunction, Series},
    utils::vec::unique_vec,
};

use super::{
    super::{equivalences::RewritingRuleDebug, Formula},
    error::Error,
    NormalForm as NormalFormTrait,
};

/// Combination of [conjunctive terms][super::Conjunct] connected using
/// the [XOR operation][crate::connective::ExclusiveDisjunction].
pub type NormalForm<T> = Series<Term<T>, ExclusiveDisjunction>;

impl<T> From<NormalForm<T>> for Formula<T> {
    fn from(value: NormalForm<T>) -> Self {
        value.compose()
    }
}

impl<T> Evaluable for NormalForm<T> {
    type Partial = Self;

    fn terminal(value: bool) -> Self {
        if value {
            Self::new(Some(Term::tautology()))
        } else {
            Self::new(None)
        }
    }

    fn is_tautology(&self) -> bool {
        matches!(self.as_ref(), [term] if term.is_tautology())
    }

    fn is_contradiction(&self) -> bool {
        self.as_ref().is_empty()
    }

    fn partial(val: Self) -> Self {
        val
    }

    fn into_terminal(self) -> Result<bool, Self> {
        match self.as_ref() {
            [] => Ok(false),
            [term] if term.is_tautology() => Ok(true),
            _ => Err(self),
        }
    }
}

/// Combination of variables connected using
/// the [AND operation][crate::connective::Conjunction].
type Term<T> = super::Conjunct<T>;

impl<T> From<Term<T>> for Formula<T> {
    fn from(term: Term<T>) -> Self {
        term.compose()
    }
}

impl<T: PartialEq> Term<T> {
    fn from_plain_vars(formula: Formula<T>) -> Result<Option<Self>, Error> {
        match formula {
            Formula::TruthValue(t) => Ok(t.then(Self::tautology)),
            Formula::Atomic(x) => Ok(Some(Self::new(Some(x)))),
            Formula::And(f1, f2) => {
                let term1 = Self::from_plain_vars(*f1)?;
                let term2 = Self::from_plain_vars(*f2)?;

                let result = term1.and_then(|term1| {
                    term2.map(|term2| {
                        Self::new(unique_vec(term1.into_iter().chain(term2).collect()))
                    })
                });
                Ok(result)
            }
            Formula::Not(_)
            | Formula::Or(_, _)
            | Formula::Xor(_, _)
            | Formula::Implies(_, _)
            | Formula::Equivalent(_, _)
            | Formula::Dynamic(_) => Err(Error::InvalidConnective {
                normal_form: std::any::type_name::<Self>(),
                connective: formula.get_connective().map(|_| ()),
            }),
        }
    }
}

impl<T> NormalForm<T>
where
    T: PartialEq,
{
    fn convert_prepared(formula: Formula<T>) -> Result<Self, Error> {
        let nf = match formula {
            Formula::TruthValue(false) => Self::contradiction(),
            Formula::Xor(f1, f2) => {
                let nf1 = Self::convert_prepared(*f1)?;
                let nf2 = Self::convert_prepared(*f2)?;

                Self::xor(nf1.into_iter().chain(nf2).collect())
            }
            other => {
                let term = Term::from_plain_vars(other)?;
                Self::new(term)
            }
        };
        Ok(nf)
    }

    /// Create a [`NormalForm`] from a number of terms.
    pub fn xor(terms: Vec<Term<T>>) -> Self {
        let mut counted_terms: Vec<(Term<T>, usize)> = Vec::new();

        for term in terms {
            let found_counter = counted_terms
                .iter_mut()
                .find_map(|(t, count)| (t == &term).then_some(count));
            if let Some(counter) = found_counter {
                *counter += 1;
            } else {
                counted_terms.push((term, 1));
            }
        }

        let cleaned = counted_terms
            .into_iter()
            .filter_map(|(term, count)| (count % 2 == 1).then_some(term));

        Self::new(cleaned)
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
            // no `Formula::Dynamic`
            Box::new(canonical::NoDynamicConnective),
            // do not eliminate constants to preserve `⊤`
            //Box::new(constant::EliminateConstants),
            // no `Formula::Not(Formula::Not(...))`
            Box::new(neg::DoubleNegation),
            // reintroduce `⊤` for `Formula::Not`
            Box::new(xor::NegLiteralAsXor1),
            // no `Formula::Implies`
            Box::new(xor::Implication),
            // no `Formula::Equivalent`
            Box::new(xor::Equiv),
            // no `Formula::And(Formula::Xor(...))`
            Box::new(distrib::DistributeConjunctionOverXor),
            // no `Formula::Or`
            Box::new(xor::DisjunctionToXoredProducts),
            // additional rule to prevent repetitions early
            // Box::new(eq::Idempotence),
        ]
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
    }

    #[test]
    fn conjunction_with_truth() {
        let f = Formula::truth(true) & 'a';
        let nf = NormalForm::convert_prepared(f).unwrap();

        let term = Term::new(Some('a'));
        assert_eq!(nf, NormalForm::new(Some(term)));
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
