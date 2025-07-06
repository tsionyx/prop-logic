use super::{super::formula::Formula, RewritingRule};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Eliminate disjunctions from a [`Formula`] by converting them
/// to a combination of XOR-ed conjunctions.
///
/// `p ∨ q ≡ p ⊕ q ⊕ (p ∧ q)`
///
/// This transformation is useful to convert a [`Formula`] into _algebraic normal form_.
pub struct DisjunctionToXoredProducts;

impl<T: Clone> RewritingRule<T> for DisjunctionToXoredProducts {
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        Err(formula)
    }

    fn transform(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        if let Formula::Or(p, q) = formula {
            Ok(p.clone() ^ q.clone() ^ (p & q))
        } else {
            Err(formula)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Replace an implication in a [`Formula`] with a disjunction:
///
/// p → q ≡ ¬p ∨ q ≡ (p ⊕ ⊤) ∨ q ≡ (p ⊕ ⊤) ⊕ q ⊕ ((p ∧ q) ⊕ (⊤ ∧ q)) ≡ p ⊕ ⊤ ⊕ q ⊕ (p ∧ q) ⊕ q ≡ p ⊕ ⊤ ⊕ (p ∧ q)
///
/// This transformation is useful to convert a [`Formula`] into _algebraic normal form_.
pub struct Implication;

impl<T: Clone> RewritingRule<T> for Implication {
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        Err(formula)
    }

    fn transform(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        if let Formula::Implies(p, q) = formula {
            Ok(!p.clone() ^ (p & q))
        } else {
            Err(formula)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Replace an equivalence in a [`Formula`]
/// with a negation of exclusive disjunctions:
/// `p ↔ q ≡ ¬(p ⊕ q)`
pub struct Equiv;

impl<T> RewritingRule<T> for Equiv {
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        if let Formula::Equivalent(p, q) = formula {
            Ok(!(p ^ q))
        } else {
            Err(formula)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Replace negative literal with a `⊕ ⊤`:
///
/// `¬p ≡ p ⊕ ⊤`
///
/// This transformation is useful to convert a [`Formula`] into _algebraic normal form_.
pub struct NegLiteralAsXor1;

impl<T> RewritingRule<T> for NegLiteralAsXor1 {
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        Err(formula)
    }

    fn transform(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        if let Formula::Not(n) = formula {
            Ok(Formula::truth(true) ^ *n)
        } else {
            Err(formula)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::TruthTabled as _;

    use super::{super::examples::reduce_all, *};

    #[allow(clippy::needless_pass_by_value)]
    fn check_equivalent(f: Formula<char>) {
        use crate::formula::normal_forms::{AlgebraicNormalForm, NormalForm as _};
        let rules = AlgebraicNormalForm::<()>::rules();
        let f2 = reduce_all(f.clone(), true, rules);
        assert!(f2.is_equivalent(&f));
    }

    #[test]
    fn example1() {
        use crate::formula::Equivalent as _;

        let f = Formula::atom('a').equivalent(Formula::atom('b') & (Formula::atom('c') | 'd'));
        check_equivalent(f);
    }

    #[test]
    fn example2() {
        use crate::formula::{Equivalent as _, Implies as _};

        // ¬a↔(d→c)∧a
        let f = (!Formula::atom('a')).equivalent(Formula::atom('d').implies('c') & 'a');
        check_equivalent(f);
    }

    #[test]
    fn example3() {
        use crate::{
            connective::{NonConjunction, NonDisjunction},
            formula::{AnyConnective, Equivalent as _, Implies as _},
            Literal,
        };

        // ((¬b∧d)⊕((b↑b)↓d))∨(((c→d)→a)↔¬(¬c∨(d∨¬d)))
        let f = ((!Formula::atom('b') & 'd')
            ^ AnyConnective::binary(
                NonDisjunction,
                (
                    AnyConnective::binary(NonConjunction, (Formula::atom('b'), Formula::atom('b')))
                        .into(),
                    Formula::atom('d'),
                ),
            ))
            | (Formula::atom('c')
                .implies('d')
                .implies('a')
                .equivalent(!(!Formula::atom('c') | (Formula::atom('d') | Literal::Neg('d')))));
        check_equivalent(f);
    }

    #[test]
    fn example4() {
        use crate::{
            connective::{ConverseImplication, MaterialNonImplication, NonDisjunction},
            formula::{AnyConnective, Equivalent as _, Implies as _},
            Literal,
        };

        // (((c↓⊥)↛c)↛¬(⊥←a))∨(((b→a)↔¬b)∨(d∧¬⊥))
        let f = Formula::from(AnyConnective::binary(
            MaterialNonImplication,
            (
                AnyConnective::binary(
                    MaterialNonImplication,
                    (
                        AnyConnective::binary(
                            NonDisjunction,
                            (Formula::atom('c'), Formula::truth(false)),
                        )
                        .into(),
                        Formula::atom('c'),
                    ),
                )
                .into(),
                !Formula::from(AnyConnective::binary(
                    ConverseImplication,
                    (Formula::truth(false), Formula::atom('a')),
                )),
            ),
        )) | (Formula::atom('b')
            .implies('a')
            .equivalent(Literal::Neg('b'))
            | (Formula::atom('d') & !Formula::truth(false)));

        check_equivalent(f);
    }

    #[test]
    fn example5() {
        use crate::{
            connective::{
                ConverseImplication, MaterialNonImplication, NonConjunction, NonDisjunction,
            },
            formula::{AnyConnective, Equivalent as _},
            Literal,
        };

        // (a∨c↛a)∨¬d←(¬b↔(d↓a))↑¬((b∨a)∨¬a)
        let f = AnyConnective::binary(
            NonConjunction,
            (
                AnyConnective::binary(
                    ConverseImplication,
                    (
                        Formula::from(AnyConnective::binary(
                            MaterialNonImplication,
                            (Formula::atom('a') | 'c', Formula::atom('a')),
                        )) | !Formula::atom('d'),
                        (!Formula::atom('b')).equivalent(AnyConnective::binary(
                            NonDisjunction,
                            (Formula::atom('d'), Formula::atom('a')),
                        )),
                    ),
                )
                .into(),
                !(Formula::atom('b') | 'a' | Literal::Neg('a')),
            ),
        );

        check_equivalent(f.into());
    }

    #[test]
    fn example6() {
        use crate::{
            connective::{MaterialNonImplication, NonDisjunction},
            formula::{AnyConnective, Equivalent as _, Implies as _},
        };

        // (a↔(c↛a))↛((c↔(d→b))↛(d↓c))
        let f = AnyConnective::binary(
            MaterialNonImplication,
            (
                Formula::atom('a').equivalent(AnyConnective::binary(
                    MaterialNonImplication,
                    (Formula::atom('c'), Formula::atom('a')),
                )),
                AnyConnective::binary(
                    MaterialNonImplication,
                    (
                        Formula::atom('c').equivalent(Formula::atom('d').implies('b')),
                        AnyConnective::binary(
                            NonDisjunction,
                            (Formula::atom('d'), Formula::atom('c')),
                        )
                        .into(),
                    ),
                )
                .into(),
            ),
        );

        check_equivalent(f.into());
    }

    #[test]
    fn example7() {
        use crate::{
            connective::{ConverseImplication, NonConjunction},
            formula::{AnyConnective, Equivalent as _, Implies as _},
        };

        // TODO: investigate why so slow? (~38s)

        // ((a↑b⊕a)∨(d→d))∧(((b∧⊤↔d)∧(d⊕a))∨(a→b))←((c→a)∧(d⊕b))∧((⊤↔c)→⊥)
        let f = AnyConnective::binary(
            ConverseImplication,
            (
                (Formula::from(AnyConnective::binary(
                    NonConjunction,
                    (Formula::atom('a'), Formula::atom('b') ^ 'a'),
                )) | (Formula::atom('d').implies('d')))
                    & (((Formula::atom('b') & Formula::truth(true)).equivalent('d')
                        & (Formula::atom('d') ^ 'a'))
                        | (Formula::atom('a').implies('b'))),
                (Formula::atom('c').implies('a') & (Formula::atom('d') ^ 'b'))
                    & Formula::truth(true)
                        .equivalent('c')
                        .implies(Formula::truth(false)),
            ),
        );

        check_equivalent(f.into());
    }

    // TODO:
    // (((c)↓d)∧((c∨⊤)∨((c↛b)∧(a∨a↛a))))∨((c⊕d↚((c⊕b)⊕d)∧(⊤↚d))∧(¬(¬(b∧b))→(b↚c)))
}
