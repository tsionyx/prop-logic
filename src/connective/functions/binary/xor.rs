//! Exclusive disjunction is a binary operation that
//! is `true` if and only if its arguments differ.
//!
//! <https://en.wikipedia.org/wiki/Exclusive_or>
use crate::{
    connective::Series,
    formula::{Not, Xor},
};

use super::super::{
    super::{Connective, Evaluable, FunctionNotation, TruthFn},
    neg::Negation,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Exclusive disjunction is an operation on two logical values,
/// typically the values of two propositions, that produces a value of `true`
/// if and only if one is `true` and the other is `false`.
pub struct ExclusiveDisjunction;

impl<E> TruthFn<2, E> for ExclusiveDisjunction
where
    E: Evaluable + Xor + Not,
{
    fn try_reduce(&self, [x, y]: [E; 2]) -> Result<E, [E; 2]> {
        match (x.into_terminal(), y.into_terminal()) {
            (Ok(disjunct1), Ok(disjunct2)) => Ok(E::terminal(disjunct1.xor(disjunct2))),
            // **exclusive disjunction** is _commutative_
            (Ok(val), Err(x)) | (Err(x), Ok(val)) => {
                if val {
                    Ok(Negation.compose([E::partial(x)]))
                } else {
                    Ok(E::partial(x))
                }
            }
            (Err(x), Err(y)) => Err([E::partial(x), E::partial(y)]),
        }
    }

    fn compose(&self, [x, y]: [E; 2]) -> E {
        x.xor(y)
    }
}

impl Connective<2> for ExclusiveDisjunction {
    fn notation(&self) -> FunctionNotation {
        '⊕'.into()
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            '^'.into(),
            // https://en.wikipedia.org/wiki/List_of_logic_symbols#Basic_logic_symbols
            '⊻'.into(),
            '↮'.into(),
            '≢'.into(),
            FunctionNotation::common("xor"),
            // https://en.wikipedia.org/wiki/XOR_gate
            FunctionNotation::scheme_gate("XOR"),
            FunctionNotation::Polish('J'),
        ])
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Generalization of the [`ExclusiveDisjunction`] for any number of values.
///
/// It produces a value of `true` if and only if there are _odd_ number of truthy operands,
/// otherwise the result is `false`.
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
/// - [`DisjunctionAny`][super::or::DisjunctionAny];
/// - [`AllEquivalent`][super::xnor::AllEquivalent];
pub struct ExclusiveDisjunctionAny;

impl<const ARITY: usize, E> TruthFn<ARITY, E> for ExclusiveDisjunctionAny
where
    E: Evaluable + Xor + Not,
{
    fn try_reduce(&self, terms: [E; ARITY]) -> Result<E, [E; ARITY]> {
        let tautologies = terms.iter().filter(|e| e.is_tautology()).count();
        let _contradictions = terms.iter().filter(|e| e.is_contradiction()).count();

        // odd number of tautologies equivalent to single Truth value
        let is_truth_constant = tautologies % 2 == 1;

        let partials = terms.iter().filter(|e| e.is_partial()).count();
        if partials == 0 {
            Ok(E::terminal(is_truth_constant))
        } else if partials == 1 {
            let Some(partial) = terms.into_iter().find(E::is_partial) else {
                panic!("Found on previous step");
            };

            if is_truth_constant {
                // single truth negate the result
                Ok(Negation::negate(partial))
            } else {
                Ok(partial)
            }
        } else {
            Err(terms)
        }
    }

    fn compose(&self, terms: [E; ARITY]) -> E {
        Series::<_, ExclusiveDisjunction>::new(terms).compose()
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
            &ExclusiveDisjunctionAny::init()
        ));
        assert!(LogicalIdentity.is_equivalent(&ExclusiveDisjunctionAny::init()));
        assert!(ExclusiveDisjunction.is_equivalent(&ExclusiveDisjunctionAny::init()));
        assert!(Ternary::<true, ExclusiveDisjunction>::init()
            .is_equivalent(&ExclusiveDisjunctionAny::init()));
        assert!(Ternary::<false, ExclusiveDisjunction>::init()
            .is_equivalent(&ExclusiveDisjunctionAny::init()));
    }

    #[test]
    fn any_compose() {
        use crate::Formula;

        let f: Formula<char> = ExclusiveDisjunctionAny.compose([]);
        assert_eq!(f, Formula::truth(false));

        let f: Formula<char> = ExclusiveDisjunctionAny.compose([Formula::atom('a')]);
        assert_eq!(f, Formula::atom('a'));

        let f: Formula<char> = ExclusiveDisjunctionAny.compose(['a'.into(), 'b'.into()]);
        assert_eq!(f, Formula::atom('a') ^ 'b');

        let f: Formula<char> =
            ExclusiveDisjunctionAny.compose(['a'.into(), 'b'.into(), 'c'.into()]);
        assert_eq!(f, Formula::atom('a') ^ (Formula::atom('b') ^ 'c'));
    }
}
