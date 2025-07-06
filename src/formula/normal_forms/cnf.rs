//! Optimized representation of [`Formula`][super::super::Formula]
//! in the so-called _Conjunctive normal form_.
//!
//! This representation consists of one or more terms called [Disjunct], connected
//! with the [AND operation][crate::connective::Conjunction]
//! where every disjunct is the combination of one or more literals
//! connected with the [OR operation][crate::connective::Disjunction].
//!
//! This form can also be viewed as a special case of [`Formula`][super::super::Formula]
//! in the ([functionally complete][crate::connective::is_complete]) set of:
//! - [NOT][crate::connective::Negation];
//! - [AND][crate::connective::Conjunction];
//! - [OR][crate::connective::Disjunction].
//!
//! <https://en.wikipedia.org/wiki/Conjunctive_normal_form>
use crate::connective::Evaluable;

use super::{
    super::{equivalences::RewritingRuleDebug, Formula, Signed as Literal},
    error::Error,
    NormalForm as NormalFormTrait,
};

/// Combination of [Disjunct][super::Disjunct]-s connected with
/// the [AND operation][crate::connective::Conjunction].
pub type NormalForm<T> = super::Conjunct<SumTerm<T>>;

impl<T> From<NormalForm<T>> for Formula<T> {
    fn from(value: NormalForm<T>) -> Self {
        value.compose()
    }
}

impl<T> Evaluable for NormalForm<T> {
    type Partial = Self;

    fn terminal(value: bool) -> Self {
        if value {
            Self::tautology()
        } else {
            Self::new(Some(SumTerm::contradiction()))
        }
    }

    fn is_tautology(&self) -> bool {
        Self::is_tautology(self)
    }

    fn is_contradiction(&self) -> bool {
        matches!(self.as_ref(), [term] if term.is_contradiction())
    }

    fn partial(val: Self) -> Self {
        val
    }

    fn into_terminal(self) -> Result<bool, Self> {
        match self.as_ref() {
            [] => Ok(true),
            [term] if term.is_contradiction() => Ok(false),
            _ => Err(self),
        }
    }
}

pub type SumTerm<T> = super::Disjunct<Literal<T>>;

impl<T> From<SumTerm<T>> for Formula<T> {
    fn from(value: SumTerm<T>) -> Self {
        value.compose()
    }
}

impl<T> SumTerm<T>
where
    T: PartialEq,
{
    fn convert_prepared(formula: Formula<T>) -> Result<Option<Self>, Error> {
        match formula {
            Formula::TruthValue(t) => Ok((!t).then(Self::contradiction)),
            Formula::Atomic(x) => Ok(Some(Self::new(Some(Literal::Pos(x))))),
            Formula::Not(f) => {
                if let Formula::Atomic(x) = *f {
                    Ok(Some(Self::new(Some(Literal::Neg(x)))))
                } else {
                    Err(Error::UnexpectedNot {
                        normal_form: std::any::type_name::<Self>(),
                        connective: f.get_connective().map(|_| ()),
                    })
                }
            }
            Formula::Or(f1, f2) => {
                let term1 = Self::convert_prepared(*f1)?;
                let term2 = Self::convert_prepared(*f2)?;

                let result = term1.and_then(|term1| {
                    term2.and_then(|term2| Self::or(term1.into_iter().chain(term2).collect()))
                });
                Ok(result)
            }
            Formula::And(_, _)
            | Formula::Xor(_, _)
            | Formula::Implies(_, _)
            | Formula::Equivalent(_, _)
            | Formula::Dynamic(_) => Err(Error::InvalidConnective {
                normal_form: std::any::type_name::<Self>(),
                connective: formula.get_connective().map(|_| ()),
            }),
        }
    }

    /// Create a `SumTerm` from a number of literals.
    ///
    /// # Return
    /// - `None` corresponds to constant _Truth_;
    /// - `Some` but empty `SumTerm` corresponds to _Falsity_.
    pub fn or(disjuncts: Vec<Literal<T>>) -> Option<Self> {
        let mut unique: Vec<Literal<T>> = Vec::with_capacity(disjuncts.len());

        for lit in disjuncts {
            // detect repeating variables
            let found_negated = unique.iter().any(|v| v.by_ref() == !lit.by_ref());
            // if have `p` and `¬p`, then we can reduce the whole sequence to `⊤`
            if found_negated {
                return None;
            }

            // skip repeating
            if !unique.contains(&lit) {
                unique.push(lit);
            }
        }

        Some(Self::new(unique))
    }
}

impl<T> NormalForm<T>
where
    T: PartialEq,
{
    fn convert_prepared(formula: Formula<T>) -> Result<Self, Error> {
        let nf = if let Formula::And(f1, f2) = formula {
            let nf1 = Self::convert_prepared(*f1)?;
            let nf2 = Self::convert_prepared(*f2)?;

            Self::and(nf1.into_iter().chain(nf2).collect())
        } else {
            let term = SumTerm::convert_prepared(formula)?;
            Self::new(term)
        };
        Ok(nf)
    }

    /// Create a [`NormalForm`] from a number of literals.
    pub fn and(conjuncts: Vec<SumTerm<T>>) -> Self {
        let mut unique: Vec<SumTerm<T>> = Vec::with_capacity(conjuncts.len());

        for term in conjuncts {
            // if found `⊥`, then we can reduce the whole sequence to `⊥`
            if term.is_contradiction() {
                return Self::contradiction();
            }

            let found_subset = unique
                .iter()
                .any(|v| v.as_unsorted().is_subset(term.as_unsorted()));
            // if have `A` and `B` which is a superset of `A` (or just equals to `A`)
            // then we can ignore the `B` in favor of `A`
            if !found_subset {
                unique.push(term);
            }
        }

        Self::new(unique)
    }
}

impl<T> TryFrom<Formula<T>> for NormalForm<T>
where
    T: PartialEq + Clone + 'static,
{
    type Error = Error;

    fn try_from(formula: Formula<T>) -> Result<Self, Self::Error> {
        let nnf = super::NegationNormalForm::try_from(formula)?;
        let formula = nnf.into();
        let formula = Self::prepare(formula);
        Self::convert_prepared(formula)
    }
}

type DummyType = u8;

impl<T> NormalFormTrait<T> for NormalForm<T>
where
    T: PartialEq + Clone + 'static,
{
    fn rules<V: PartialEq + Clone>() -> Vec<Box<dyn RewritingRuleDebug<V>>> {
        let mut rules = super::NegationNormalForm::<DummyType>::rules::<V>();

        // # ATENTION
        // We are here under the assumption
        // the Formula already reduced to NNF, so we only need
        // one distributivity rule to apply.
        // Attempting to enable all the previous rules for NNF
        // will result in infinite lengthening some formulae using DeMorgan rule.
        // TODO: find out why DeMorgan is applicable here
        //
        // This implies the set of rules returned by the given function is unusable on its own
        // without previous conversion to NNF (see the `TryFrom<Formula<T>>` above).
        rules.clear();

        // At this point, the formula should be in NNF form already.
        // To promote it to CNF, we need to apply one more rule
        // to force the disjunction to happen only between literals.
        let distrib_rule: Box<dyn RewritingRuleDebug<V>> =
            Box::new(super::super::equivalences::distrib::DistributeDisjunctionOverConjunction);

        rules.into_iter().chain(Some(distrib_rule)).collect()
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

        assert_eq!(nf, NormalForm::new(Some(SumTerm::new(Some('a'.into())))));
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

        assert_eq!(nf, NormalForm::new(Some(SumTerm::new(Some('a'.into())))));
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

    #[test]
    fn hard_example() {
        use crate::{formula::Implies as _, TruthTabled as _};

        // ¬((d⊕d)∨((b⊕((d→c)∨⊥))⊕d))
        let f = !((Formula::atom('d') ^ 'd')
            | ((Formula::atom('b')
                ^ ((Formula::from('d').implies('c')) | Formula::contradiction()))
                ^ 'd'));

        eprintln!("1. Original: {f:#}:\n{}", f.get_truth_table());

        let nnf = super::super::NegationNormalForm::try_from(f.clone()).unwrap();
        let nnf_formula: Formula<char> = nnf.into();
        eprintln!(
            "2. NNF: {nnf_formula:#}: \n{}",
            nnf_formula.get_truth_table()
        );

        let reduced = NormalForm::<char>::prepare(f.clone());
        eprintln!(
            "3. Reduced out of NF: {reduced:#}: \n{}",
            reduced.get_truth_table()
        );
        let nf = NormalForm::try_from(f.clone()).unwrap();
        eprintln!("4. NF: {nf}");
        let f2: Formula<char> = nf.into();
        eprintln!("5. Result: {f2:#}:\n{}", f2.get_truth_table());
        assert!(f.is_equivalent(&f2));
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
