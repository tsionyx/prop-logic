//! _Logical biconditional_, aka _material biconditional_
//! or _equivalence_ or _biimplication_ or _bientailment_,
//! is a logical operation that formally express
//! the notion of equality of its operands.
//!
//! <https://en.wikipedia.org/wiki/Logical_biconditional>
//! <https://en.wikipedia.org/wiki/Logical_equality>
use std::ops::{BitXor, Not};

use super::super::super::{Connective, Evaluable, FunctionNotation, TruthFn};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// Logical biconditional is an operation on two logical values,
/// that produces a value of `true`
/// if and only if both operands are `false` or both operands are `true`.
pub struct LogicalBiconditional;

impl<E> TruthFn<2, E> for LogicalBiconditional
where
    E: Evaluable + Not<Output = E> + BitXor<Output = E>,
{
    fn fold(&self, [x, y]: [E; 2]) -> Result<E, [E; 2]> {
        match (x.into_terminal(), y.into_terminal()) {
            (Ok(x), Ok(y)) => Ok(E::terminal(x == y)),
            // **equivalence** is _commutative_
            (Ok(val), Err(x)) | (Err(x), Ok(val)) => {
                if val {
                    Ok(E::partial(x))
                } else {
                    Ok(!E::partial(x))
                }
            }
            (Err(x), Err(y)) => Err([E::partial(x), E::partial(y)]),
        }
    }

    fn compose(&self, terms: [E; 2]) -> E {
        self.fold(terms).unwrap_or_else(|[x, y]| !(x ^ y))
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
/// It produces a value of `true` if and only if all the operands
/// share the same value (either `true` or `false`), otherwise the result is `false`.
///
/// The two degenerative cases are:
/// - nullary (ARITY=0) and unary (ARITY=1):
///   the connective is equivalent to [`Truth`][super::super::Truth];
///
/// ---
/// The non-trivial boolean operation could be interpreted as the function of arbitrary arity
/// only if it is both _commutative_ and _associative_.
/// See also:
/// - [`ConjunctionAny`][super::and::ConjunctionAny];
/// - [`DisjunctionAny`][super::or::DisjunctionAny];
/// - [`ExclusiveDisjunctionAny`][super::xor::ExclusiveDisjunctionAny];
///
/// # Attention
/// Be aware that this functions is **different** from the previous three examples
/// in terms of being the **stronger** than the simple combination
/// of multiple [binary equalities][super::xnor::AllEquivalent] because it represent
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
    E: Evaluable + Not<Output = E> + BitXor<Output = E>,
{
    fn fold(&self, terms: [E; ARITY]) -> Result<E, [E; ARITY]> {
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
                Ok(!partial)
            } else {
                // does not matter whether we have tautologies or not
                Ok(partial)
            }
        } else {
            Err(terms)
        }
    }

    fn compose(&self, terms: [E; ARITY]) -> E {
        self.fold(terms).unwrap_or_else(|terms| {
            terms
                .into_iter()
                .rfold(E::tautology(), |acc, t| match acc.into_terminal() {
                    Ok(truth_acc) => {
                        if truth_acc {
                            t
                        } else {
                            !t
                        }
                    }
                    Err(partial_acc) => match t.into_terminal() {
                        Ok(truth_term) => {
                            if truth_term {
                                E::partial(partial_acc)
                            } else {
                                !E::partial(partial_acc)
                            }
                        }
                        Err(partial_term) => !(E::partial(partial_term) ^ E::partial(partial_acc)),
                    },
                })
        })
    }
}

#[cfg(test)]
mod tests {
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
}
