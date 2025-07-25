//! Logical conjunction is a binary operation that
//! is `true` if and only if all of its operands are true.
//!
//! <https://en.wikipedia.org/wiki/Logical_conjunction>
use crate::{connective::Series, formula::And};

use super::super::super::{Connective, Evaluable, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Logical conjunction is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// if and only if both of its operands are `true`.
pub struct Conjunction;

impl<E> TruthFn<2, E> for Conjunction
where
    E: Evaluable + And,
{
    fn try_reduce(&self, [x, y]: [E; 2]) -> Result<E, [E; 2]> {
        match (x.into_terminal(), y.into_terminal()) {
            (Ok(conjunct1), Ok(conjunct2)) => Ok(E::terminal(conjunct1.and(conjunct2))),
            // **conjunction** is _commutative_
            (Ok(val), Err(x)) | (Err(x), Ok(val)) => {
                if val {
                    Ok(E::partial(x))
                } else {
                    Ok(E::contradiction())
                }
            }
            (Err(x), Err(y)) => Err([E::partial(x), E::partial(y)]),
        }
    }

    fn compose(&self, [x, y]: [E; 2]) -> E {
        x.and(y)
    }
}

impl Connective<2> for Conjunction {
    fn notation(&self) -> FunctionNotation {
        '∧'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Basic_logic_symbols
            '&'.into(),
            '×'.into(),
            '·'.into(),
            FunctionNotation::symbolic_str("&&"),
            FunctionNotation::common("and"),
            // https://en.wikipedia.org/wiki/AND_gate
            FunctionNotation::scheme_gate("AND"),
            // short for Polish `koniunkcja`
            FunctionNotation::Polish('K'),
        ])
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Generalization of the [`Conjunction`] for any number of values.
///
/// It produces a value of `true` if and only if all of its operands are `true`.
/// If at least one of its operands is `false`, then the result is `false`.
///
/// The two degenerative cases are:
/// - nullary (ARITY=0):
///   the connective is equivalent to [`Truth`][super::super::Truth];
/// - unary (ARTIY=1):
///   the connective is equivalent to [`LogicalIdentity`][super::super::LogicalIdentity];
///
/// ---
/// The non-trivial boolean operation could be interpreted as the function of arbitrary arity
/// only if it is both _commutative_ and _associative_.
/// See also:
/// - [`DisjunctionAny`][super::or::DisjunctionAny];
/// - [`ExclusiveDisjunctionAny`][super::xor::ExclusiveDisjunctionAny];
/// - [`AllEquivalent`][super::xnor::AllEquivalent];
pub struct ConjunctionAny;

impl<const ARITY: usize, E> TruthFn<ARITY, E> for ConjunctionAny
where
    E: Evaluable + And,
{
    fn try_reduce(&self, terms: [E; ARITY]) -> Result<E, [E; ARITY]> {
        if terms.iter().any(E::is_contradiction) {
            return Ok(E::contradiction());
        }

        if terms.iter().all(E::is_tautology) {
            return Ok(E::tautology());
        }

        let partials = terms.iter().filter(|e| e.is_partial()).count();
        assert!(
            partials > 0,
            "No-partials case should be catched by the previous short-circuting routines"
        );

        if partials == 1 {
            terms
                .into_iter()
                .find(E::is_partial)
                .ok_or_else(|| panic!("Found on previous step"))
        } else {
            assert!(
                partials >= 2,
                "No-partials and single-partial cases should be catched by the previous short-circuting routines"
            );
            Err(terms)
        }
    }

    fn compose(&self, terms: [E; ARITY]) -> E {
        Series::<_, Conjunction>::new(terms).compose()
    }
}

#[cfg(test)]
mod tests {
    use crate::truth_table::TruthTabled;

    use super::{
        super::super::{super::InitFn as _, ternary::Ternary, LogicalIdentity, Truth},
        *,
    };

    #[test]
    fn any_equivalences() {
        assert!(<Truth as TruthTabled<0>>::is_equivalent(
            &Truth,
            &ConjunctionAny::init()
        ));
        assert!(LogicalIdentity.is_equivalent(&ConjunctionAny::init()));
        assert!(Conjunction.is_equivalent(&ConjunctionAny::init()));
        assert!(Ternary::<true, Conjunction>::init().is_equivalent(&ConjunctionAny::init()));
        assert!(Ternary::<false, Conjunction>::init().is_equivalent(&ConjunctionAny::init()));
    }

    #[test]
    fn any_compose() {
        use crate::Formula;

        let f: Formula<char> = ConjunctionAny.compose([]);
        assert_eq!(f, Formula::truth(true));

        let f: Formula<char> = ConjunctionAny.compose([Formula::atom('a')]);
        assert_eq!(f, Formula::atom('a'));

        let f: Formula<char> = ConjunctionAny.compose(['a'.into(), 'b'.into()]);
        assert_eq!(f, Formula::atom('a') & 'b');

        let f: Formula<char> = ConjunctionAny.compose(['a'.into(), 'b'.into(), 'c'.into()]);
        assert_eq!(f, Formula::atom('a') & (Formula::atom('b') & 'c'));
    }
}
