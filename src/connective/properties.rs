use std::{any::Any, collections::BTreeMap as Map};

use itertools::Itertools;

use super::BoolFn;

use crate::{
    arity::two_powers,
    utils::{
        dependent_array::{CheckedArray, Discriminant},
        upcast::Upcast,
    },
};

/// Defines the important properies of a [`BoolFn`]
/// based on its [`TruthTable`][crate::connective::TruthTable].
///
/// The most important of the properies are the includeness
/// of a boolean function in some of the
/// [Post's lattices](https://en.wikipedia.org/wiki/Post%27s_lattice),
///especially the ones required to detect
/// the [functional completeness](https://en.wikipedia.org/wiki/Functional_completeness).
///
/// Also, some other properties introduced, that could describe the function's behaviour.
/// See more at <https://en.wikipedia.org/wiki/Boolean_function#Properties>.
///
/// Requires [`std::any::Any`] as a supertrait to enable `Any::type_id` in dyn context.
pub trait BoolFnExt<const ARITY: usize>: BoolFn<ARITY> + Upcast<dyn BoolFn<ARITY>> + Any {
    /// The constant connective is always *T* or always *F* regardless of its arguments.
    fn is_constant(&self) -> bool;

    /// The falsity-preserving connective return the truth value *F*
    /// under any interpretation that assigns *F* to all variables.
    fn is_falsity_preserving(&self) -> bool;

    /// The truth-preserving connective return the truth value *T*
    /// under any interpretation that assigns *T* to all variables.
    fn is_truth_preserving(&self) -> bool;

    /// The monotonic connective never change its return value
    /// from *T* to *F* when changing the truth value of any connected variables
    /// from *F* to *T* without changing any from *T* to *F*.
    fn is_monotonic(&self) -> bool;

    /// The affine connective's return value always or never
    /// depends on changing each connected variable's truth value.
    fn is_affine(&self) -> bool;

    /// The parity function has the truth value *T*
    /// if and only if it has the odd number of *T* in its input.
    ///
    /// The parity property is essentially
    /// defines the generalization of the **XOR** function for any number of arguments.
    ///
    /// <https://en.wikipedia.org/wiki/Parity_function>
    fn is_parity(&self) -> bool;

    /// The Hamming weight of the connective is the number of ones in the truth table.
    fn hamming_weight(&self) -> usize;

    /// The truth table of the balanced connective
    /// contains an equal number of zeros and ones.
    fn is_balanced(&self) -> bool;

    /// The truth value of the evasive connective
    /// depends on all of its arguments.
    ///
    /// This property could also be called _weakly evasive_
    /// as it requires the values of all the variables to evaluate **in the worst case**.
    /// The evasiveness defined this way could also be interpreted
    /// as the non-ability to reduce the function to the function of the smaller _ARITY_,
    /// i.e. the decision path for such a function has the **maximum** length of _ARITY_.
    /// This also implies that the non-weakly-evasive function always could be stripped off
    /// some non-significant variable(s).
    ///
    /// For the fully-evasive property, take a look
    /// at the (stronger) [`Self::is_fully_evasive`] property.
    ///
    /// <https://en.wikipedia.org/wiki/Evasive_Boolean_function>
    fn is_evasive(&self) -> bool;

    /// The truth value of the evasive connective
    /// **always** depends on all of its arguments.
    ///
    /// This property could also be called _truly evasive_
    /// as it requires the values of all the variables to evaluate **in every possible case**.
    /// The evasiveness defined this way could also be interpreted
    /// as the non-ability to short-cut the function's evaluation for any values of the arguments,
    /// i.e. the decision path for such a function **always** has the length of _ARITY_.
    ///
    /// For the weakly-evasive property, take a look
    /// at the (weaker) [`Self::is_evasive`] property.
    fn is_fully_evasive(&self) -> bool;

    /// The self-dual connective is equal
    /// to its own [de Morgan dual](https://en.wikipedia.org/wiki/De_Morgan_dual)
    /// I.e. the returned truth value gets reversed if the truth values of all variables are reversed.
    fn is_self_dual(&self) -> bool;

    /// Tells whether the function is a
    /// [_sole sufficient operator_](https://en.wikipedia.org/wiki/Functional_completeness#Minimal_functionally_complete_operator_sets)
    /// i.e. it can be used to generate any formula only by using it alone.
    ///
    /// Defined using the Post's criterion about **not holding** the five properties:
    /// - [falsity preserving][Self::is_falsity_preserving];
    /// - [truth preserving][Self::is_truth_preserving];
    /// - [monotonness][Self::is_monotonic];
    /// - [affinness][Self::is_affine];
    /// - [self duality][Self::is_self_dual].
    ///
    /// More of it: <https://en.wikipedia.org/wiki/Functional_completeness#Characterization_of_functional_completeness>.
    fn is_sheffer(&self) -> bool {
        !self.is_falsity_preserving()
            && !self.is_truth_preserving()
            && !self.is_monotonic()
            && !self.is_affine()
            && !self.is_self_dual()
    }
}

impl<const ARITY: usize, T> BoolFnExt<ARITY> for T
where
    T: BoolFn<ARITY> + 'static,
    two_powers::D: CheckedArray<ARITY>,
{
    fn is_constant(&self) -> bool {
        self.get_truth_table()
            .into_iter()
            .map(|(_, val)| val)
            .all_equal()
    }

    fn is_falsity_preserving(&self) -> bool {
        !self.eval([false; ARITY])
    }

    fn is_truth_preserving(&self) -> bool {
        self.eval([true; ARITY])
    }

    fn is_monotonic(&self) -> bool {
        let truth_table: Map<_, _> = self.get_truth_table().into_iter().collect();
        truth_table.iter().all(|(&vars, &value)| {
            if value {
                vars.into_iter().enumerate().all(|(i, var)| {
                    if var {
                        // skip monotonicity check if value of an argument is already *T*
                        true
                    } else {
                        let mut flipped = vars;
                        // changig the value *F* -> *T* should not change
                        // the value of a function *T* -> *F*
                        flipped[i] = true;
                        truth_table.get(&flipped).copied() == Some(value)
                    }
                })
            } else {
                // no need to check monotonicity for *F* value
                true
            }
        })
    }

    fn is_affine(&self) -> bool {
        let truth_table: Map<_, _> = self.get_truth_table().into_iter().collect();
        let affine = (0..ARITY).all(|i| {
            // check that i-th variable always or never affect the result
            truth_table
                .iter()
                .map(|(&vars, &value)| {
                    let mut flipped = vars;
                    flipped[i] = !flipped[i];
                    truth_table.get(&flipped).copied() == Some(value)
                })
                .all_equal()
        });

        if !affine {
            debug_assert!(!self.is_balanced(), "Not affine but balanced");
        }
        affine
    }

    fn is_parity(&self) -> bool {
        let truth_table = self.get_truth_table();
        truth_table.into_iter().all(|(vars, value)| {
            let truths_number = vars.iter().filter(|x| **x).count();
            let odd_truths = truths_number % 2 == 1;
            odd_truths == value
        })
    }

    fn hamming_weight(&self) -> usize {
        self.get_truth_table()
            .into_iter()
            .map(|(_, val)| val)
            .filter(|x| *x)
            .count()
    }

    fn is_balanced(&self) -> bool {
        let expected_size = <two_powers::D as Discriminant<ARITY>>::ARR_SIZE;
        let balanced = self.hamming_weight() * 2 == expected_size;
        if balanced {
            // TODO: consider checking the
            // [Post's lattice](https://en.wikipedia.org/wiki/Post%27s_lattice)
            // for more implications between classes
            debug_assert!(self.is_affine(), "Balanced but not affine");
        } else {
            debug_assert!(!self.is_self_dual(), "Not balanced but self-dual");
        }
        balanced
    }

    fn is_evasive(&self) -> bool {
        let truth_table: Map<_, _> = self.get_truth_table().into_iter().collect();
        let evasive = (0..ARITY).all(|i| {
            // check that i-th variable affects the result at least once
            truth_table.iter().any(|(&vars, &value)| {
                let mut flipped = vars;
                flipped[i] = !flipped[i];
                truth_table.get(&flipped).copied() == Some(!value)
            })
        });

        let hamming_weight = self.hamming_weight();
        if hamming_weight % 2 == 1 {
            // if a Boolean function has an odd number of combinations
            // of arguments for which it is true, then it must be evasive
            debug_assert!(
                evasive,
                "Not evasive for odd hamming weight {hamming_weight}",
            );
        }

        if !evasive {
            debug_assert!(
                !self.is_fully_evasive(),
                "Not weakly evasive but fully evasive",
            );
        }

        evasive
    }

    fn is_fully_evasive(&self) -> bool {
        let truth_table: Map<_, _> = self.get_truth_table().into_iter().collect();
        let truly_evasive = (0..ARITY).all(|i| {
            // check that i-th variable always affects the result
            truth_table.iter().all(|(&vars, &value)| {
                let mut flipped = vars;
                flipped[i] = !flipped[i];
                truth_table.get(&flipped).copied() == Some(!value)
            })
        });

        if truly_evasive {
            debug_assert!(self.is_evasive(), "Fully evasive but not weakly evasive",);
        }

        truly_evasive
    }

    fn is_self_dual(&self) -> bool {
        let truth_table: Map<_, _> = self.get_truth_table().into_iter().collect();
        let self_dual = truth_table.iter().all(|(&vars, &value)| {
            let inversed_vars = vars.map(|x| !x);
            truth_table.get(&inversed_vars).copied() == Some(!value)
        });

        if self_dual {
            debug_assert!(self.is_balanced(), "Self-dual but not balanced");
        }
        self_dual
    }
}

#[cfg(test)]
mod tests {
    use super::{
        super::{functions::*, TruthFn},
        *,
    };

    macro_rules! assert_prop {
        ($F:ty: $prop:ident) => {
            assert!(<$F>::init().$prop())
        };
        ($F:ty, $arity:literal: $prop:ident) => {
            assert!(<$F as BoolFnExt<$arity>>::$prop(
                &<$F as TruthFn<$arity>>::init()
            ))
        };

        ($F:ty: ! $prop:ident) => {
            assert!(!<$F>::init().$prop())
        };
        ($F:ty, $arity:literal: ! $prop:ident) => {
            assert!(!<$F as BoolFnExt<$arity>>::$prop(&<$F as TruthFn<
                $arity,
            >>::init()))
        };
    }

    #[test]
    fn test_all_nullary_constant() {
        assert_prop!(Falsity, 0: is_constant);
        assert_prop!(Truth, 0: is_constant);
    }

    #[test]
    fn test_all_unary_constant() {
        assert_prop!(Falsity, 1: is_constant);
        assert_prop!(LogicalIdentity: ! is_constant);
        assert_prop!(Negation: ! is_constant);
        assert_prop!(Truth, 1: is_constant);
    }

    #[test]
    fn test_all_binary_constant() {
        assert_prop!(Falsity, 2: is_constant);
        assert_prop!(Conjunction: ! is_constant);
        assert_prop!(MaterialNonImplication: ! is_constant);
        assert_prop!(Projection<0>: ! is_constant);
        assert_prop!(ConverseNonImplication: ! is_constant);
        assert_prop!(Projection<1>: ! is_constant);
        assert_prop!(ExclusiveDisjunction: ! is_constant);
        assert_prop!(Disjunction: ! is_constant);
        assert_prop!(NonDisjunction: ! is_constant);
        assert_prop!(LogicalBiconditional: ! is_constant);
        assert_prop!(ProjectAndUnary<1, Negation>: ! is_constant);
        assert_prop!(ConverseImplication: ! is_constant);
        assert_prop!(ProjectAndUnary<0, Negation>: ! is_constant);
        assert_prop!(MaterialImplication: ! is_constant);
        assert_prop!(NonConjunction: ! is_constant);
        assert_prop!(Truth, 2: is_constant);
    }

    #[test]
    fn test_all_nullary_falsity_preserving() {
        assert_prop!(Falsity, 0: is_falsity_preserving);
        assert_prop!(Truth, 0: ! is_falsity_preserving);
    }

    #[test]
    fn test_all_unary_falsity_preserving() {
        assert_prop!(Falsity, 1: is_falsity_preserving);
        assert_prop!(LogicalIdentity: is_falsity_preserving);
        assert_prop!(Negation: ! is_falsity_preserving);
        assert_prop!(Truth, 1: ! is_falsity_preserving);
    }

    #[test]
    fn test_all_binary_falsity_preserving() {
        assert_prop!(Falsity, 2: is_falsity_preserving);
        assert_prop!(Conjunction: is_falsity_preserving);
        assert_prop!(MaterialNonImplication: is_falsity_preserving);
        assert_prop!(Projection<0>: is_falsity_preserving);
        assert_prop!(ConverseNonImplication: is_falsity_preserving);
        assert_prop!(Projection<1>: is_falsity_preserving);
        assert_prop!(ExclusiveDisjunction: is_falsity_preserving);
        assert_prop!(Disjunction: is_falsity_preserving);
        assert_prop!(NonDisjunction: ! is_falsity_preserving);
        assert_prop!(LogicalBiconditional: ! is_falsity_preserving);
        assert_prop!(ProjectAndUnary<1, Negation>: ! is_falsity_preserving);
        assert_prop!(ConverseImplication: ! is_falsity_preserving);
        assert_prop!(ProjectAndUnary<0, Negation>: ! is_falsity_preserving);
        assert_prop!(MaterialImplication: ! is_falsity_preserving);
        assert_prop!(NonConjunction: ! is_falsity_preserving);
        assert_prop!(Truth, 2: ! is_falsity_preserving);
    }

    #[test]
    fn test_all_nullary_truth_preserving() {
        assert_prop!(Falsity, 0: ! is_truth_preserving);
        assert_prop!(Truth, 0: is_truth_preserving);
    }

    #[test]
    fn test_all_unary_truth_preserving() {
        assert_prop!(Falsity, 1: ! is_truth_preserving);
        assert_prop!(LogicalIdentity: is_truth_preserving);
        assert_prop!(Negation: ! is_truth_preserving);
        assert_prop!(Truth, 1: is_truth_preserving);
    }

    #[test]
    fn test_all_binary_truth_preserving() {
        assert_prop!(Falsity, 2: ! is_truth_preserving);
        assert_prop!(Conjunction: is_truth_preserving);
        assert_prop!(MaterialNonImplication: ! is_truth_preserving);
        assert_prop!(Projection<0>: is_truth_preserving);
        assert_prop!(ConverseNonImplication: ! is_truth_preserving);
        assert_prop!(Projection<1>: is_truth_preserving);
        assert_prop!(ExclusiveDisjunction: ! is_truth_preserving);
        assert_prop!(Disjunction: is_truth_preserving);
        assert_prop!(NonDisjunction: ! is_truth_preserving);
        assert_prop!(LogicalBiconditional: is_truth_preserving);
        assert_prop!(ProjectAndUnary<1, Negation>: ! is_truth_preserving);
        assert_prop!(ConverseImplication: is_truth_preserving);
        assert_prop!(ProjectAndUnary<0, Negation>: ! is_truth_preserving);
        assert_prop!(MaterialImplication: is_truth_preserving);
        assert_prop!(NonConjunction: ! is_truth_preserving);
        assert_prop!(Truth, 2: is_truth_preserving);
    }

    #[test]
    fn test_all_nullary_monotonic() {
        assert_prop!(Falsity, 0: is_monotonic);
        assert_prop!(Truth, 0: is_monotonic);
    }

    #[test]
    fn test_all_unary_monotonic() {
        assert_prop!(Falsity, 1: is_monotonic);
        assert_prop!(LogicalIdentity: is_monotonic);
        assert_prop!(Negation: ! is_monotonic);
        assert_prop!(Truth, 1: is_monotonic);
    }

    #[test]
    fn test_all_binary_monotonic() {
        assert_prop!(Falsity, 2: is_monotonic);
        assert_prop!(Conjunction: is_monotonic);
        assert_prop!(MaterialNonImplication: ! is_monotonic);
        assert_prop!(Projection<0>: is_monotonic);
        assert_prop!(ConverseNonImplication: ! is_monotonic);
        assert_prop!(Projection<1>: is_monotonic);
        assert_prop!(ExclusiveDisjunction: ! is_monotonic);
        assert_prop!(Disjunction: is_monotonic);
        assert_prop!(NonDisjunction: ! is_monotonic);
        assert_prop!(LogicalBiconditional: ! is_monotonic);
        assert_prop!(ProjectAndUnary<1, Negation>: ! is_monotonic);
        assert_prop!(ConverseImplication: ! is_monotonic);
        assert_prop!(ProjectAndUnary<0, Negation>: ! is_monotonic);
        assert_prop!(MaterialImplication: ! is_monotonic);
        assert_prop!(NonConjunction: ! is_monotonic);
        assert_prop!(Truth, 2: is_monotonic);
    }

    #[test]
    fn test_all_nullary_affine() {
        assert_prop!(Falsity, 0: is_affine);
        assert_prop!(Truth, 0: is_affine);
    }

    #[test]
    fn test_all_unary_affine() {
        assert_prop!(Falsity, 1: is_affine);
        assert_prop!(LogicalIdentity: is_affine);
        assert_prop!(Negation: is_affine);
        assert_prop!(Truth, 1: is_affine);
    }

    #[test]
    fn test_all_binary_affine() {
        assert_prop!(Falsity, 2: is_affine);
        assert_prop!(Conjunction: ! is_affine);
        assert_prop!(MaterialNonImplication: ! is_affine);
        assert_prop!(Projection<0>: is_affine);
        assert_prop!(ConverseNonImplication: ! is_affine);
        assert_prop!(Projection<1>: is_affine);
        assert_prop!(ExclusiveDisjunction: is_affine);
        assert_prop!(Disjunction: ! is_affine);
        assert_prop!(NonDisjunction: ! is_affine);
        assert_prop!(LogicalBiconditional: is_affine);
        assert_prop!(ProjectAndUnary<1, Negation>: is_affine);
        assert_prop!(ConverseImplication: ! is_affine);
        assert_prop!(ProjectAndUnary<0, Negation>: is_affine);
        assert_prop!(MaterialImplication: ! is_affine);
        assert_prop!(NonConjunction: ! is_affine);
        assert_prop!(Truth, 2: is_affine);
    }

    #[test]
    fn test_all_nullary_parity() {
        assert_prop!(Falsity, 0: is_parity);
        assert_prop!(Truth, 0: ! is_parity);
    }

    #[test]
    fn test_all_unary_parity() {
        assert_prop!(Falsity, 1: ! is_parity);
        assert_prop!(LogicalIdentity: is_parity);
        assert_prop!(Negation: ! is_parity);
        assert_prop!(Truth, 1: ! is_parity);
    }

    #[test]
    fn test_all_binary_parity() {
        assert_prop!(Falsity, 2: ! is_parity);
        assert_prop!(Conjunction: ! is_parity);
        assert_prop!(MaterialNonImplication: ! is_parity);
        assert_prop!(Projection<0>: ! is_parity);
        assert_prop!(ConverseNonImplication: ! is_parity);
        assert_prop!(Projection<1>: ! is_parity);
        assert_prop!(ExclusiveDisjunction: is_parity);
        assert_prop!(Disjunction: ! is_parity);
        assert_prop!(NonDisjunction: ! is_parity);
        assert_prop!(LogicalBiconditional: ! is_parity);
        assert_prop!(ProjectAndUnary<1, Negation>: ! is_parity);
        assert_prop!(ConverseImplication: ! is_parity);
        assert_prop!(ProjectAndUnary<0, Negation>: ! is_parity);
        assert_prop!(MaterialImplication: ! is_parity);
        assert_prop!(NonConjunction: ! is_parity);
        assert_prop!(Truth, 2: ! is_parity);
    }

    #[test]
    fn test_all_nullary_balanced() {
        assert_prop!(Falsity, 0: ! is_balanced);
        assert_prop!(Truth, 0: ! is_balanced);
    }

    #[test]
    fn test_all_unary_balanced() {
        assert_prop!(Falsity, 1: ! is_balanced);
        assert_prop!(LogicalIdentity: is_balanced);
        assert_prop!(Negation: is_balanced);
        assert_prop!(Truth, 1: ! is_balanced);
    }

    #[test]
    fn test_all_binary_balanced() {
        assert_prop!(Falsity, 2: ! is_balanced);
        assert_prop!(Conjunction: ! is_balanced);
        assert_prop!(MaterialNonImplication: ! is_balanced);
        assert_prop!(Projection<0>: is_balanced);
        assert_prop!(ConverseNonImplication: ! is_balanced);
        assert_prop!(Projection<1>: is_balanced);
        assert_prop!(ExclusiveDisjunction: is_balanced);
        assert_prop!(Disjunction: ! is_balanced);
        assert_prop!(NonDisjunction: ! is_balanced);
        assert_prop!(LogicalBiconditional: is_balanced);
        assert_prop!(ProjectAndUnary<1, Negation>: is_balanced);
        assert_prop!(ConverseImplication: ! is_balanced);
        assert_prop!(ProjectAndUnary<0, Negation>: is_balanced);
        assert_prop!(MaterialImplication: ! is_balanced);
        assert_prop!(NonConjunction: ! is_balanced);
        assert_prop!(Truth, 2: ! is_balanced);
    }

    #[test]
    fn test_all_nullary_evasive() {
        assert_prop!(Falsity, 0: is_evasive);
        assert_prop!(Truth, 0: is_evasive);
    }

    #[test]
    fn test_all_unary_evasive() {
        assert_prop!(Falsity, 1: ! is_evasive);
        assert_prop!(LogicalIdentity: is_evasive);
        assert_prop!(Negation: is_evasive);
        assert_prop!(Truth, 1: ! is_evasive);
    }

    #[test]
    fn test_all_binary_evasive() {
        assert_prop!(Falsity, 2: ! is_evasive);
        assert_prop!(Conjunction: is_evasive);
        assert_prop!(MaterialNonImplication: is_evasive);
        assert_prop!(Projection<0>: ! is_evasive);
        assert_prop!(ConverseNonImplication: is_evasive);
        assert_prop!(Projection<1>: ! is_evasive);
        assert_prop!(ExclusiveDisjunction: is_evasive);
        assert_prop!(Disjunction: is_evasive);
        assert_prop!(NonDisjunction: is_evasive);
        assert_prop!(LogicalBiconditional: is_evasive);
        assert_prop!(ProjectAndUnary<1, Negation>: ! is_evasive);
        assert_prop!(ConverseImplication: is_evasive);
        assert_prop!(ProjectAndUnary<0, Negation>: ! is_evasive);
        assert_prop!(MaterialImplication: is_evasive);
        assert_prop!(NonConjunction: is_evasive);
        assert_prop!(Truth, 2: ! is_evasive);
    }

    #[test]
    fn test_all_nullary_fully_evasive() {
        assert_prop!(Falsity, 0: is_fully_evasive);
        assert_prop!(Truth, 0: is_fully_evasive);
    }

    #[test]
    fn test_all_unary_fully_evasive() {
        assert_prop!(Falsity, 1: ! is_fully_evasive);
        assert_prop!(LogicalIdentity: is_fully_evasive);
        assert_prop!(Negation: is_fully_evasive);
        assert_prop!(Truth, 1: ! is_fully_evasive);
    }

    #[test]
    fn test_all_binary_fully_evasive() {
        assert_prop!(Falsity, 2: ! is_fully_evasive);
        assert_prop!(Conjunction: ! is_fully_evasive);
        assert_prop!(MaterialNonImplication: ! is_fully_evasive);
        assert_prop!(Projection<0>: ! is_fully_evasive);
        assert_prop!(ConverseNonImplication: ! is_fully_evasive);
        assert_prop!(Projection<1>: ! is_fully_evasive);
        assert_prop!(ExclusiveDisjunction: is_fully_evasive);
        assert_prop!(Disjunction: ! is_fully_evasive);
        assert_prop!(NonDisjunction: ! is_fully_evasive);
        assert_prop!(LogicalBiconditional: is_fully_evasive);
        assert_prop!(ProjectAndUnary<1, Negation>: ! is_fully_evasive);
        assert_prop!(ConverseImplication: ! is_fully_evasive);
        assert_prop!(ProjectAndUnary<0, Negation>: ! is_fully_evasive);
        assert_prop!(MaterialImplication: ! is_fully_evasive);
        assert_prop!(NonConjunction: ! is_fully_evasive);
        assert_prop!(Truth, 2: ! is_fully_evasive);
    }

    #[test]
    fn test_all_nullary_self_dual() {
        assert_prop!(Falsity, 0: ! is_self_dual);
        assert_prop!(Truth, 0: ! is_self_dual);
    }

    #[test]
    fn test_all_unary_self_dual() {
        assert_prop!(Falsity, 1: ! is_self_dual);
        assert_prop!(LogicalIdentity: is_self_dual);
        assert_prop!(Negation: is_self_dual);
        assert_prop!(Truth, 1: ! is_self_dual);
    }

    #[test]
    fn test_all_binary_self_dual() {
        assert_prop!(Falsity, 2: ! is_self_dual);
        assert_prop!(Conjunction: ! is_self_dual);
        assert_prop!(MaterialNonImplication: ! is_self_dual);
        assert_prop!(Projection<0>: is_self_dual);
        assert_prop!(ConverseNonImplication: ! is_self_dual);
        assert_prop!(Projection<1>: is_self_dual);
        assert_prop!(ExclusiveDisjunction: ! is_self_dual);
        assert_prop!(Disjunction: ! is_self_dual);
        assert_prop!(NonDisjunction: ! is_self_dual);
        assert_prop!(LogicalBiconditional: ! is_self_dual);
        assert_prop!(ProjectAndUnary<1, Negation>: is_self_dual);
        assert_prop!(ConverseImplication: ! is_self_dual);
        assert_prop!(ProjectAndUnary<0, Negation>: is_self_dual);
        assert_prop!(MaterialImplication: ! is_self_dual);
        assert_prop!(NonConjunction: ! is_self_dual);
        assert_prop!(Truth, 2: ! is_self_dual);
    }

    #[test]
    fn test_all_nullary_sheffer() {
        assert_prop!(Falsity, 0: ! is_sheffer);
        assert_prop!(Truth, 0: ! is_sheffer);
    }

    #[test]
    fn test_all_unary_sheffer() {
        assert_prop!(Falsity, 1: ! is_sheffer);
        assert_prop!(LogicalIdentity: ! is_sheffer);
        assert_prop!(Negation: ! is_sheffer);
        assert_prop!(Truth, 1: ! is_sheffer);
    }

    #[test]
    fn test_all_binary_sheffer() {
        assert_prop!(Falsity, 2: ! is_sheffer);
        assert_prop!(Conjunction: ! is_sheffer);
        assert_prop!(MaterialNonImplication: ! is_sheffer);
        assert_prop!(Projection<0>: ! is_sheffer);
        assert_prop!(ConverseNonImplication: ! is_sheffer);
        assert_prop!(Projection<1>: ! is_sheffer);
        assert_prop!(ExclusiveDisjunction: ! is_sheffer);
        assert_prop!(Disjunction: ! is_sheffer);
        assert_prop!(NonDisjunction: is_sheffer);
        assert_prop!(LogicalBiconditional: ! is_sheffer);
        assert_prop!(ProjectAndUnary<1, Negation>: ! is_sheffer);
        assert_prop!(ConverseImplication: ! is_sheffer);
        assert_prop!(ProjectAndUnary<0, Negation>: ! is_sheffer);
        assert_prop!(MaterialImplication: ! is_sheffer);
        assert_prop!(NonConjunction: is_sheffer);
        assert_prop!(Truth, 2: ! is_sheffer);
    }
}
