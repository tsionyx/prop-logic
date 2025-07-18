//! Logical disjunction is a binary operation that
//! is `true` when either or both of its operands are true.
//!
//! <https://en.wikipedia.org/wiki/Logical_disjunction>
use crate::{connective::Series, formula::Or};

use super::super::super::{Connective, Evaluable, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Logical disjunction is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// unless both of its arguments are `false`.
pub struct Disjunction;

impl<E> TruthFn<2, E> for Disjunction
where
    E: Evaluable + Or,
{
    fn try_reduce(&self, [x, y]: [E; 2]) -> Result<E, [E; 2]> {
        match (x.into_terminal(), y.into_terminal()) {
            (Ok(disjunct1), Ok(disjunct2)) => Ok(E::terminal(disjunct1.or(disjunct2))),
            // **disjunction** is _commutative_
            (Ok(val), Err(x)) | (Err(x), Ok(val)) => {
                if val {
                    Ok(E::tautology())
                } else {
                    Ok(E::partial(x))
                }
            }
            (Err(x), Err(y)) => Err([E::partial(x), E::partial(y)]),
        }
    }

    fn compose(&self, [x, y]: [E; 2]) -> E {
        x.or(y)
    }
}

impl Connective<2> for Disjunction {
    fn notation(&self) -> FunctionNotation {
        '∨'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Basic_logic_symbols
            '+'.into(),
            '∥'.into(),
            FunctionNotation::symbolic_str("||"),
            FunctionNotation::common("or"),
            // https://en.wikipedia.org/wiki/OR_gate
            FunctionNotation::scheme_gate("OR"),
            // short for Polish `alternatywa`
            FunctionNotation::Polish('A'),
        ])
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Generalization of the [`Disjunction`] for any number of values.
///
/// It produces a value of `false` if and only if all of its operands are `false`.
/// If at least one of its operands is `true`, then the result is `true`.
///
/// The two degenerative cases are:
/// - nullary (ARITY=0):
///   the connective is equivalent to [`Falsity`][super::super::Falsity];
/// - unary (ARTIY=1):
///   the connective is equivalent to [`LogicalIdentity`][super::super::LogicalIdentity];
///
/// ---
/// The non-trivial boolean operation could be interpreted as the function of arbitrary arity
/// only if it is both _commutative_ and _associative_.
/// See also:
/// - [`ConjunctionAny`][super::and::ConjunctionAny];
/// - [`ExclusiveDisjunctionAny`][super::xor::ExclusiveDisjunctionAny];
/// - [`AllEquivalent`][super::xnor::AllEquivalent];
pub struct DisjunctionAny;

impl<const ARITY: usize, E> TruthFn<ARITY, E> for DisjunctionAny
where
    E: Evaluable + Or,
{
    fn try_reduce(&self, terms: [E; ARITY]) -> Result<E, [E; ARITY]> {
        if terms.iter().any(E::is_tautology) {
            return Ok(E::tautology());
        }

        if terms.iter().all(E::is_contradiction) {
            return Ok(E::contradiction());
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
        Series::<_, Disjunction>::new(terms).compose()
    }
}

#[cfg(test)]
mod tests {
    use crate::truth_table::TruthTabled;

    use super::{
        super::super::{super::InitFn as _, ternary::Ternary, Falsity, LogicalIdentity},
        *,
    };

    #[test]
    fn any_equivalences() {
        assert!(<Falsity as TruthTabled<0>>::is_equivalent(
            &Falsity,
            &DisjunctionAny::init()
        ));
        assert!(LogicalIdentity.is_equivalent(&DisjunctionAny::init()));
        assert!(Disjunction.is_equivalent(&DisjunctionAny::init()));
        assert!(Ternary::<true, Disjunction>::init().is_equivalent(&DisjunctionAny::init()));
        assert!(Ternary::<false, Disjunction>::init().is_equivalent(&DisjunctionAny::init()));
    }

    #[test]
    fn any_compose() {
        use crate::Formula;

        let f: Formula<char> = DisjunctionAny.compose([]);
        assert_eq!(f, Formula::truth(false));

        let f: Formula<char> = DisjunctionAny.compose([Formula::atom('a')]);
        assert_eq!(f, Formula::atom('a'));

        let f: Formula<char> = DisjunctionAny.compose(['a'.into(), 'b'.into()]);
        assert_eq!(f, Formula::atom('a') | 'b');

        let f: Formula<char> = DisjunctionAny.compose(['a'.into(), 'b'.into(), 'c'.into()]);
        assert_eq!(f, Formula::atom('a') | (Formula::atom('b') | 'c'));
    }
}
