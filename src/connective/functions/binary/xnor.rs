//! _Logical biconditional_, aka _material biconditional_
//! or _equivalence_ or _biimplication_ or _bientailment_,
//! is a logical operation that formally express
//! the notion of equality of its operands.
//!
//! <https://en.wikipedia.org/wiki/Logical_biconditional>
//! <https://en.wikipedia.org/wiki/Logical_equality>
use crate::{
    connective::Series,
    formula::{Equivalent, Not},
};

use super::super::{
    super::{Connective, Evaluable, FunctionNotation, TruthFn},
    neg::Negation,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Logical biconditional is an operation on two logical values,
/// that produces a value of `true`
/// if and only if both operands are `false` or both operands are `true`.
pub struct LogicalBiconditional;

impl<E> TruthFn<2, E> for LogicalBiconditional
where
    E: Evaluable + Equivalent + Not,
{
    fn try_reduce(&self, [x, y]: [E; 2]) -> Result<E, [E; 2]> {
        match (x.into_terminal(), y.into_terminal()) {
            (Ok(x), Ok(y)) => Ok(E::terminal(x.equivalent(y))),
            // **equivalence** is _commutative_
            (Ok(val), Err(x)) | (Err(x), Ok(val)) => {
                if val {
                    Ok(E::partial(x))
                } else {
                    Ok(Negation.compose([E::partial(x)]))
                }
            }
            (Err(x), Err(y)) => Err([E::partial(x), E::partial(y)]),
        }
    }

    fn compose(&self, [x, y]: [E; 2]) -> E {
        x.equivalent(y)
    }
}

impl Connective<2> for LogicalBiconditional {
    fn notation(&self) -> FunctionNotation {
        '↔'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '='.into(),
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Basic_logic_symbols
            '⇔'.into(),
            '≡'.into(),
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Advanced_or_rarely_used_logical_symbols
            '⊙'.into(),
            FunctionNotation::symbolic_str("⊃⊂"),
            FunctionNotation::symbolic_str("⊂⊃"),
            FunctionNotation::common("iff"),
            FunctionNotation::common("eq"),
            // https://en.wikipedia.org/wiki/XOR_gate
            FunctionNotation::scheme_gate("XNOR"),
            // short for Polish `ekwiwalencja` (1929)
            FunctionNotation::Polish('E'),
            // alternate Polish notation (1951)
            FunctionNotation::Polish('Q'),
        ])
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Generalization of the [`LogicalBiconditional`] for any number of values.
///
/// It produces a value of `true` if and only if there are _even_ number of falsy operands,
/// otherwise the result is `false`.
///
/// The two degenerative cases are:
/// - nullary (ARITY=0):
///   the connective is equivalent to [`Truth`][super::super::Truth];
/// - unary (ARTIY=1):
///   the connective is equivalent to [`LogicalIdentity`][super::super::LogicalIdentity];
/// ---
/// The non-trivial boolean operation could be interpreted as the function of arbitrary arity
/// only if it is both _commutative_ and _associative_.
/// See also:
/// - [`ConjunctionAny`][super::and::ConjunctionAny];
/// - [`DisjunctionAny`][super::or::DisjunctionAny];
/// - [`ExclusiveDisjunctionAny`][super::xor::ExclusiveDisjunctionAny];
pub struct EquivalentAny;

impl<const ARITY: usize, E> TruthFn<ARITY, E> for EquivalentAny
where
    E: Evaluable + Equivalent + Not,
{
    fn try_reduce(&self, terms: [E; ARITY]) -> Result<E, [E; ARITY]> {
        let _tautologies = terms.iter().filter(|e| e.is_tautology()).count();
        let contradictions = terms.iter().filter(|e| e.is_contradiction()).count();

        // even number of contradictions equivalent to single Truth value
        let is_truth_constant = contradictions % 2 == 0;

        let partials = terms.iter().filter(|e| e.is_partial()).count();
        if partials == 0 {
            Ok(E::terminal(is_truth_constant))
        } else if partials == 1 {
            let Some(partial) = terms.into_iter().find(E::is_partial) else {
                panic!("Found on previous step");
            };

            if is_truth_constant {
                Ok(partial)
            } else {
                Ok(Negation::negate(partial))
            }
        } else {
            Err(terms)
        }
    }

    fn compose(&self, terms: [E; ARITY]) -> E {
        Series::<_, LogicalBiconditional>::new(terms).compose()
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Generalization of the [`LogicalBiconditional`] for any number of values.
///
/// It produces a value of `true` if and only if all the operands
/// share the same value (either `true` or `false`), otherwise the result is `false`.
///
/// The two degenerative cases are:
/// - nullary (ARITY=0) and unary (ARITY=1):
///   the connective is equivalent to [`Truth`][super::super::Truth];
///
/// # Attention
/// Be aware that this functions is **different** from the [`EquivalentAny`]
/// in terms of being the **stronger** than the simple combination
/// of multiple [binary equalities][LogicalBiconditional] because it represent
/// the 'all equal' relationship rather than the 'pairwise equal',
/// i.e.:
///
/// `(a == b) == c` is of course the same as
/// `b == (c == a)`
/// because of the commutativity and associativity of the binary equality.
///
/// But they are **not the same** as truly ternary `== (a, b, c)`
/// which is the **all equal** relationship.
pub struct AllEquivalent;

impl<const ARITY: usize, E> TruthFn<ARITY, E> for AllEquivalent
where
    E: Evaluable + Equivalent + Not,
{
    fn try_reduce(&self, terms: [E; ARITY]) -> Result<E, [E; ARITY]> {
        if terms.iter().all(E::is_tautology) {
            return Ok(E::tautology());
        }

        if terms.iter().all(E::is_contradiction) {
            return Ok(E::tautology());
        }

        let have_tautologies = terms.iter().any(E::is_tautology);
        let have_contradictions = terms.iter().any(E::is_contradiction);
        if have_tautologies && have_contradictions {
            return Ok(E::contradiction());
        }

        let partials = terms.iter().filter(|e| e.is_partial()).count();
        assert!(
            partials > 0,
            "No-partials case should be catched by the previous short-circuting routines"
        );

        if partials == 1 {
            let Some(partial) = terms.into_iter().find(E::is_partial) else {
                panic!("Found on previous step");
            };

            if have_contradictions {
                Ok(Negation::negate(partial))
            } else {
                // does not matter whether we have tautologies or not
                Ok(partial)
            }
        } else {
            Err(terms)
        }
    }

    fn compose(&self, terms: [E; ARITY]) -> E {
        if ARITY < 2 {
            return E::tautology();
        }

        terms
            .into_iter()
            .rfold(E::tautology(), |acc, t| match acc.into_terminal() {
                Ok(truth_acc) => {
                    if truth_acc {
                        t
                    } else {
                        Negation.compose([t])
                    }
                }
                Err(partial_acc) => match t.into_terminal() {
                    Ok(truth_term) => {
                        if truth_term {
                            E::partial(partial_acc)
                        } else {
                            Negation.compose([E::partial(partial_acc)])
                        }
                    }
                    Err(partial_term) => {
                        E::partial(partial_term).equivalent(E::partial(partial_acc))
                    }
                },
            })
    }
}

#[cfg(test)]
mod tests_any {
    use crate::truth_table::TruthTabled;

    use super::{
        super::super::{super::InitFn as _, ternary::Ternary, LogicalIdentity, Truth},
        *,
    };

    #[test]
    fn any_equivalences() {
        assert!(<Truth as TruthTabled<0>>::is_equivalent(
            &Truth,
            &EquivalentAny::init()
        ));
        assert!(LogicalIdentity.is_equivalent(&EquivalentAny::init()));
        assert!(LogicalBiconditional.is_equivalent(&EquivalentAny::init()));
    }

    #[test]
    fn ternary_eq_equivalent_to_pairwise_equal() {
        assert!(Ternary::<true, LogicalBiconditional>::init().is_equivalent(&EquivalentAny::init()));
        assert!(
            Ternary::<false, LogicalBiconditional>::init().is_equivalent(&EquivalentAny::init())
        );
    }

    #[test]
    fn any_compose() {
        use crate::Formula;

        let f: Formula<char> = EquivalentAny.compose([]);
        assert_eq!(f, Formula::truth(true));

        let f: Formula<char> = EquivalentAny.compose([Formula::atom('a')]);
        assert_eq!(f, Formula::atom('a'));

        let f: Formula<char> = EquivalentAny.compose(['a'.into(), 'b'.into()]);
        assert_eq!(f, Formula::atom('a').equivalent('b'));

        let f: Formula<char> = EquivalentAny.compose(['a'.into(), 'b'.into(), 'c'.into()]);
        assert_eq!(
            f,
            Formula::atom('a').equivalent(Formula::atom('b').equivalent('c'))
        );
    }
}

#[cfg(test)]
mod tests_all {
    use crate::truth_table::TruthTabled;

    use super::{
        super::super::{super::InitFn as _, ternary::Ternary, Truth},
        *,
    };

    #[test]
    fn any_equivalences() {
        assert!(<Truth as TruthTabled<0>>::is_equivalent(
            &Truth,
            &AllEquivalent::init()
        ));
        assert!(<Truth as TruthTabled<1>>::is_equivalent(
            &Truth,
            &AllEquivalent::init()
        ));
        assert!(LogicalBiconditional.is_equivalent(&AllEquivalent::init()));
    }

    #[test]
    fn all_equal_not_equivalent_to_pairwise_equal() {
        // the 'all equal' is stronger than 'pairwise equal'
        assert!(
            !Ternary::<true, LogicalBiconditional>::init().is_equivalent(&AllEquivalent::init())
        );
        assert!(
            !Ternary::<false, LogicalBiconditional>::init().is_equivalent(&AllEquivalent::init())
        );
    }

    #[test]
    fn any_compose() {
        use crate::Formula;

        let f: Formula<char> = AllEquivalent.compose([]);
        assert_eq!(f, Formula::truth(true));

        let f: Formula<char> = AllEquivalent.compose([Formula::atom('a')]);
        assert_eq!(f, Formula::truth(true));

        let f: Formula<char> = AllEquivalent.compose(['a'.into(), 'b'.into()]);
        assert_eq!(f, Formula::atom('a').equivalent('b'));

        let f: Formula<char> = AllEquivalent.compose(['a'.into(), 'b'.into(), 'c'.into()]);
        assert_eq!(
            f,
            Formula::atom('a').equivalent(Formula::atom('b').equivalent('c'))
        );
    }
}
