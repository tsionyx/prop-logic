use std::{any::Any, collections::BTreeMap as Map};

use itertools::Itertools;

use super::BoolFn;

use crate::{
    arity::two_powers,
    utils::{
        dependent_array::{CheckedArray, Discriminant as _},
        upcast::{Upcast, UpcastFrom},
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

impl<const ARITY: usize, T: BoolFnExt<ARITY>> UpcastFrom<T> for dyn BoolFnExt<ARITY> {
    fn up_from(value: &T) -> &Self {
        value
    }

    fn up_from_mut(value: &mut T) -> &mut Self {
        value
    }
}

/// Tells whether the system of functions is functionally complete
/// i.e. the functions from this set can be used to generate any formula.
///
/// This test could produce positive outcome for redundant set of functions,
/// i.e. some of the functions could be unnecessary to get the completeness.
/// To get more accurate result, use the [`is_basis`] with
/// the additional check that no function could be removed from the set
/// without losing the completeness property.
///
/// Defined using the Post's criterion about **not holding** the five properties:
/// - [falsity preserving][BoolFnExt::is_falsity_preserving];
/// - [truth preserving][BoolFnExt::is_truth_preserving];
/// - [monotonness][BoolFnExt::is_monotonic];
/// - [affinness][BoolFnExt::is_affine];
/// - [self duality][BoolFnExt::is_self_dual].
///
/// More of it: <https://en.wikipedia.org/wiki/Functional_completeness#Characterization_of_functional_completeness>.
pub fn is_complete<const ARITY: usize>(functions: &[&dyn BoolFnExt<ARITY>]) -> bool {
    functions.iter().any(|&f| !f.is_falsity_preserving())
        && functions.iter().any(|&f| !f.is_truth_preserving())
        && functions.iter().any(|&f| !f.is_monotonic())
        && functions.iter().any(|&f| !f.is_affine())
        && functions.iter().any(|&f| !f.is_self_dual())
}

/// Tells whether the system of functions is functionally complete
/// i.e. the functions from this set can be used to generate any formula.
///
/// Also, the additional check is made that ensures
/// no function could be removed from the set
/// without losing the completeness property.
///
/// Defined using the Post's criterion about **not holding** the five properties:
/// - [falsity preserving][BoolFnExt::is_falsity_preserving];
/// - [truth preserving][BoolFnExt::is_truth_preserving];
/// - [monotonness][BoolFnExt::is_monotonic];
/// - [affinness][BoolFnExt::is_affine];
/// - [self duality][BoolFnExt::is_self_dual].
///
/// More of it: <https://en.wikipedia.org/wiki/Functional_completeness#Characterization_of_functional_completeness>.
pub fn is_basis<const ARITY: usize>(functions: &[&dyn BoolFnExt<ARITY>]) -> bool {
    let complete = is_complete(functions);
    if complete {
        (0..functions.len()).all(|index| {
            let smaller: Vec<_> = functions
                .iter()
                .enumerate()
                .filter_map(|(i, f)| (i != index).then_some(*f))
                .collect();
            !is_complete(&smaller)
        })
    } else {
        false
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
        !self.compose([false; ARITY])
    }

    fn is_truth_preserving(&self) -> bool {
        self.compose([true; ARITY])
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
        let expected_size = two_powers::D::ARR_SIZE;
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
        super::{functions::*, InitFn},
        *,
    };

    macro_rules! assert_prop {
        ($F:ty: $prop:ident) => {
            assert!(<$F>::init().$prop())
        };
        ($F:ty, $arity:literal: $prop:ident) => {
            assert!(<$F as BoolFnExt<$arity>>::$prop(&<$F as InitFn>::init()))
        };

        ($F:ty: ! $prop:ident) => {
            assert!(!<$F>::init().$prop())
        };
        ($F:ty, $arity:literal: ! $prop:ident) => {
            assert!(!<$F as BoolFnExt<$arity>>::$prop(&<$F as InitFn>::init()))
        };
    }

    #[test]
    fn test_all_nullary_constant() {
        assert_prop!(Falsity, 0: is_constant);

        assert_prop!(DisjunctionAny, 0: is_constant);
        assert_prop!(ExclusiveDisjunctionAny, 0: is_constant);
        assert_prop!(AllEquivalent, 0: is_constant);
        assert_prop!(ConjunctionAny, 0: is_constant);

        assert_prop!(Truth, 0: is_constant);
    }

    #[test]
    fn test_all_unary_constant() {
        assert_prop!(Falsity, 1: is_constant);
        assert_prop!(LogicalIdentity: ! is_constant);

        assert_prop!(DisjunctionAny, 1: !is_constant);
        assert_prop!(ExclusiveDisjunctionAny, 1: !is_constant);
        assert_prop!(ConjunctionAny, 1: !is_constant);

        assert_prop!(Negation: ! is_constant);
        assert_prop!(Truth, 1: is_constant);

        assert_prop!(AllEquivalent, 1: is_constant);
    }

    #[test]
    fn test_all_binary_constant() {
        assert_prop!(Falsity, 2: is_constant);
        assert_prop!(Conjunction: ! is_constant);
        assert_prop!(ConjunctionAny, 2: ! is_constant);
        assert_prop!(MaterialNonImplication: ! is_constant);
        assert_prop!(First: ! is_constant);
        assert_prop!(ConverseNonImplication: ! is_constant);
        assert_prop!(Last: ! is_constant);
        assert_prop!(ExclusiveDisjunction: ! is_constant);
        assert_prop!(ExclusiveDisjunctionAny, 2: ! is_constant);
        assert_prop!(Disjunction: ! is_constant);
        assert_prop!(DisjunctionAny, 2: ! is_constant);
        assert_prop!(NonDisjunction: ! is_constant);
        assert_prop!(LogicalBiconditional: ! is_constant);
        assert_prop!(AllEquivalent, 2: ! is_constant);
        assert_prop!(NotSecond: ! is_constant);
        assert_prop!(ConverseImplication: ! is_constant);
        assert_prop!(NotFirst: ! is_constant);
        assert_prop!(MaterialImplication: ! is_constant);
        assert_prop!(NonConjunction: ! is_constant);
        assert_prop!(Truth, 2: is_constant);
    }

    #[test]
    fn test_all_nullary_falsity_preserving() {
        assert_prop!(Falsity, 0: is_falsity_preserving);

        assert_prop!(DisjunctionAny, 0: is_falsity_preserving);
        assert_prop!(ExclusiveDisjunctionAny, 0: is_falsity_preserving);
        assert_prop!(AllEquivalent, 0: ! is_falsity_preserving);
        assert_prop!(ConjunctionAny, 0: ! is_falsity_preserving);

        assert_prop!(Truth, 0: ! is_falsity_preserving);
    }

    #[test]
    fn test_all_unary_falsity_preserving() {
        assert_prop!(Falsity, 1: is_falsity_preserving);
        assert_prop!(LogicalIdentity: is_falsity_preserving);

        assert_prop!(DisjunctionAny, 1: is_falsity_preserving);
        assert_prop!(ExclusiveDisjunctionAny, 1: is_falsity_preserving);
        assert_prop!(ConjunctionAny, 1: is_falsity_preserving);

        assert_prop!(Negation: ! is_falsity_preserving);
        assert_prop!(Truth, 1: ! is_falsity_preserving);

        assert_prop!(AllEquivalent, 1: ! is_falsity_preserving);
    }

    #[test]
    fn test_all_binary_falsity_preserving() {
        assert_prop!(Falsity, 2: is_falsity_preserving);
        assert_prop!(Conjunction: is_falsity_preserving);
        assert_prop!(ConjunctionAny, 2: is_falsity_preserving);
        assert_prop!(MaterialNonImplication: is_falsity_preserving);
        assert_prop!(First: is_falsity_preserving);
        assert_prop!(ConverseNonImplication: is_falsity_preserving);
        assert_prop!(Last: is_falsity_preserving);
        assert_prop!(ExclusiveDisjunction: is_falsity_preserving);
        assert_prop!(ExclusiveDisjunctionAny, 2: is_falsity_preserving);
        assert_prop!(Disjunction: is_falsity_preserving);
        assert_prop!(DisjunctionAny, 2: is_falsity_preserving);
        assert_prop!(NonDisjunction: ! is_falsity_preserving);
        assert_prop!(LogicalBiconditional: ! is_falsity_preserving);
        assert_prop!(AllEquivalent, 2: ! is_falsity_preserving);
        assert_prop!(NotSecond: ! is_falsity_preserving);
        assert_prop!(ConverseImplication: ! is_falsity_preserving);
        assert_prop!(NotFirst: ! is_falsity_preserving);
        assert_prop!(MaterialImplication: ! is_falsity_preserving);
        assert_prop!(NonConjunction: ! is_falsity_preserving);
        assert_prop!(Truth, 2: ! is_falsity_preserving);
    }

    #[test]
    fn test_all_nullary_truth_preserving() {
        assert_prop!(Falsity, 0: ! is_truth_preserving);

        assert_prop!(DisjunctionAny, 0: ! is_truth_preserving);
        assert_prop!(ExclusiveDisjunctionAny, 0: ! is_truth_preserving);
        assert_prop!(AllEquivalent, 0: is_truth_preserving);
        assert_prop!(ConjunctionAny, 0: is_truth_preserving);

        assert_prop!(Truth, 0: is_truth_preserving);
    }

    #[test]
    fn test_all_unary_truth_preserving() {
        assert_prop!(Falsity, 1: ! is_truth_preserving);
        assert_prop!(LogicalIdentity: is_truth_preserving);

        assert_prop!(DisjunctionAny, 1: is_truth_preserving);
        assert_prop!(ExclusiveDisjunctionAny, 1: is_truth_preserving);
        assert_prop!(ConjunctionAny, 1: is_truth_preserving);

        assert_prop!(Negation: ! is_truth_preserving);
        assert_prop!(Truth, 1: is_truth_preserving);

        assert_prop!(AllEquivalent, 1: is_truth_preserving);
    }

    #[test]
    fn test_all_binary_truth_preserving() {
        assert_prop!(Falsity, 2: ! is_truth_preserving);
        assert_prop!(Conjunction: is_truth_preserving);
        assert_prop!(ConjunctionAny, 2: is_truth_preserving);
        assert_prop!(MaterialNonImplication: ! is_truth_preserving);
        assert_prop!(First: is_truth_preserving);
        assert_prop!(ConverseNonImplication: ! is_truth_preserving);
        assert_prop!(Last: is_truth_preserving);
        assert_prop!(ExclusiveDisjunction: ! is_truth_preserving);
        assert_prop!(ExclusiveDisjunctionAny, 2: ! is_truth_preserving);
        assert_prop!(Disjunction: is_truth_preserving);
        assert_prop!(DisjunctionAny, 2: is_truth_preserving);
        assert_prop!(NonDisjunction: ! is_truth_preserving);
        assert_prop!(LogicalBiconditional: is_truth_preserving);
        assert_prop!(AllEquivalent, 2: is_truth_preserving);
        assert_prop!(NotSecond: ! is_truth_preserving);
        assert_prop!(ConverseImplication: is_truth_preserving);
        assert_prop!(NotFirst: ! is_truth_preserving);
        assert_prop!(MaterialImplication: is_truth_preserving);
        assert_prop!(NonConjunction: ! is_truth_preserving);
        assert_prop!(Truth, 2: is_truth_preserving);
    }

    #[test]
    fn test_all_nullary_monotonic() {
        assert_prop!(Falsity, 0: is_monotonic);

        assert_prop!(DisjunctionAny, 0: is_monotonic);
        assert_prop!(ExclusiveDisjunctionAny, 0: is_monotonic);
        assert_prop!(AllEquivalent, 0: is_monotonic);
        assert_prop!(ConjunctionAny, 0: is_monotonic);

        assert_prop!(Truth, 0: is_monotonic);
    }

    #[test]
    fn test_all_unary_monotonic() {
        assert_prop!(Falsity, 1: is_monotonic);
        assert_prop!(LogicalIdentity: is_monotonic);

        assert_prop!(DisjunctionAny, 1: is_monotonic);
        assert_prop!(ExclusiveDisjunctionAny, 1: is_monotonic);
        assert_prop!(ConjunctionAny, 1: is_monotonic);

        assert_prop!(Negation: ! is_monotonic);
        assert_prop!(Truth, 1: is_monotonic);

        assert_prop!(AllEquivalent, 1: is_monotonic);
    }

    #[test]
    fn test_all_binary_monotonic() {
        assert_prop!(Falsity, 2: is_monotonic);
        assert_prop!(Conjunction: is_monotonic);
        assert_prop!(ConjunctionAny, 2: is_monotonic);
        assert_prop!(MaterialNonImplication: ! is_monotonic);
        assert_prop!(First: is_monotonic);
        assert_prop!(ConverseNonImplication: ! is_monotonic);
        assert_prop!(Last: is_monotonic);
        assert_prop!(ExclusiveDisjunction: ! is_monotonic);
        assert_prop!(ExclusiveDisjunctionAny, 2: ! is_monotonic);
        assert_prop!(Disjunction: is_monotonic);
        assert_prop!(DisjunctionAny, 2: is_monotonic);
        assert_prop!(NonDisjunction: ! is_monotonic);
        assert_prop!(LogicalBiconditional: ! is_monotonic);
        assert_prop!(AllEquivalent, 2: ! is_monotonic);
        assert_prop!(NotSecond: ! is_monotonic);
        assert_prop!(ConverseImplication: ! is_monotonic);
        assert_prop!(NotFirst: ! is_monotonic);
        assert_prop!(MaterialImplication: ! is_monotonic);
        assert_prop!(NonConjunction: ! is_monotonic);
        assert_prop!(Truth, 2: is_monotonic);
    }

    #[test]
    fn test_all_nullary_affine() {
        assert_prop!(Falsity, 0: is_affine);

        assert_prop!(DisjunctionAny, 0: is_affine);
        assert_prop!(ExclusiveDisjunctionAny, 0: is_affine);
        assert_prop!(AllEquivalent, 0: is_affine);
        assert_prop!(ConjunctionAny, 0: is_affine);

        assert_prop!(Truth, 0: is_affine);
    }

    #[test]
    fn test_all_unary_affine() {
        assert_prop!(Falsity, 1: is_affine);
        assert_prop!(LogicalIdentity: is_affine);

        assert_prop!(DisjunctionAny, 1: is_affine);
        assert_prop!(ExclusiveDisjunctionAny, 1: is_affine);
        assert_prop!(ConjunctionAny, 1: is_affine);

        assert_prop!(Negation: is_affine);
        assert_prop!(Truth, 1: is_affine);

        assert_prop!(AllEquivalent, 1: is_affine);
    }

    #[test]
    fn test_all_binary_affine() {
        assert_prop!(Falsity, 2: is_affine);
        assert_prop!(Conjunction: ! is_affine);
        assert_prop!(ConjunctionAny, 2: ! is_affine);
        assert_prop!(MaterialNonImplication: ! is_affine);
        assert_prop!(First: is_affine);
        assert_prop!(ConverseNonImplication: ! is_affine);
        assert_prop!(Last: is_affine);
        assert_prop!(ExclusiveDisjunction: is_affine);
        assert_prop!(ExclusiveDisjunctionAny, 2: is_affine);
        assert_prop!(Disjunction: ! is_affine);
        assert_prop!(DisjunctionAny, 2: ! is_affine);
        assert_prop!(NonDisjunction: ! is_affine);
        assert_prop!(LogicalBiconditional: is_affine);
        assert_prop!(AllEquivalent, 2: is_affine);
        assert_prop!(NotSecond: is_affine);
        assert_prop!(ConverseImplication: ! is_affine);
        assert_prop!(NotFirst: is_affine);
        assert_prop!(MaterialImplication: ! is_affine);
        assert_prop!(NonConjunction: ! is_affine);
        assert_prop!(Truth, 2: is_affine);
    }

    #[test]
    fn test_all_nullary_parity() {
        assert_prop!(Falsity, 0: is_parity);

        assert_prop!(DisjunctionAny, 0: is_parity);
        assert_prop!(ExclusiveDisjunctionAny, 0: is_parity);
        assert_prop!(AllEquivalent, 0: ! is_parity);
        assert_prop!(ConjunctionAny, 0: ! is_parity);

        assert_prop!(Truth, 0: ! is_parity);
    }

    #[test]
    fn test_all_unary_parity() {
        assert_prop!(Falsity, 1: ! is_parity);
        assert_prop!(LogicalIdentity: is_parity);

        assert_prop!(DisjunctionAny, 1: is_parity);
        assert_prop!(ExclusiveDisjunctionAny, 1: is_parity);
        assert_prop!(ConjunctionAny, 1: is_parity);

        assert_prop!(Negation: ! is_parity);
        assert_prop!(Truth, 1: ! is_parity);

        assert_prop!(AllEquivalent, 1: ! is_parity);
    }

    #[test]
    fn test_all_binary_parity() {
        assert_prop!(Falsity, 2: ! is_parity);
        assert_prop!(Conjunction: ! is_parity);
        assert_prop!(ConjunctionAny, 2: ! is_parity);
        assert_prop!(MaterialNonImplication: ! is_parity);
        assert_prop!(First: ! is_parity);
        assert_prop!(ConverseNonImplication: ! is_parity);
        assert_prop!(Last: ! is_parity);
        assert_prop!(ExclusiveDisjunction: is_parity);
        assert_prop!(ExclusiveDisjunctionAny, 2: is_parity);
        assert_prop!(Disjunction: ! is_parity);
        assert_prop!(DisjunctionAny, 2: ! is_parity);
        assert_prop!(NonDisjunction: ! is_parity);
        assert_prop!(LogicalBiconditional: ! is_parity);
        assert_prop!(AllEquivalent, 2: ! is_parity);
        assert_prop!(NotSecond: ! is_parity);
        assert_prop!(ConverseImplication: ! is_parity);
        assert_prop!(NotFirst: ! is_parity);
        assert_prop!(MaterialImplication: ! is_parity);
        assert_prop!(NonConjunction: ! is_parity);
        assert_prop!(Truth, 2: ! is_parity);
    }

    #[test]
    fn test_all_nullary_balanced() {
        assert_prop!(Falsity, 0: ! is_balanced);

        assert_prop!(DisjunctionAny, 0: ! is_balanced);
        assert_prop!(ExclusiveDisjunctionAny, 0: ! is_balanced);
        assert_prop!(AllEquivalent, 0: ! is_balanced);
        assert_prop!(ConjunctionAny, 0: ! is_balanced);

        assert_prop!(Truth, 0: ! is_balanced);
    }

    #[test]
    fn test_all_unary_balanced() {
        assert_prop!(Falsity, 1: ! is_balanced);
        assert_prop!(LogicalIdentity: is_balanced);

        assert_prop!(DisjunctionAny, 1: is_balanced);
        assert_prop!(ExclusiveDisjunctionAny, 1: is_balanced);
        assert_prop!(ConjunctionAny, 1: is_balanced);

        assert_prop!(Negation: is_balanced);
        assert_prop!(Truth, 1: ! is_balanced);

        assert_prop!(AllEquivalent, 1: ! is_balanced);
    }

    #[test]
    fn test_all_binary_balanced() {
        assert_prop!(Falsity, 2: ! is_balanced);
        assert_prop!(Conjunction: ! is_balanced);
        assert_prop!(ConjunctionAny, 2: ! is_balanced);
        assert_prop!(MaterialNonImplication: ! is_balanced);
        assert_prop!(First: is_balanced);
        assert_prop!(ConverseNonImplication: ! is_balanced);
        assert_prop!(Last: is_balanced);
        assert_prop!(ExclusiveDisjunction: is_balanced);
        assert_prop!(ExclusiveDisjunctionAny, 2: is_balanced);
        assert_prop!(Disjunction: ! is_balanced);
        assert_prop!(DisjunctionAny, 2: ! is_balanced);
        assert_prop!(NonDisjunction: ! is_balanced);
        assert_prop!(LogicalBiconditional: is_balanced);
        assert_prop!(AllEquivalent, 2: is_balanced);
        assert_prop!(NotSecond: is_balanced);
        assert_prop!(ConverseImplication: ! is_balanced);
        assert_prop!(NotFirst: is_balanced);
        assert_prop!(MaterialImplication: ! is_balanced);
        assert_prop!(NonConjunction: ! is_balanced);
        assert_prop!(Truth, 2: ! is_balanced);
    }

    #[test]
    fn test_all_nullary_evasive() {
        assert_prop!(Falsity, 0: is_evasive);

        assert_prop!(DisjunctionAny, 0: is_evasive);
        assert_prop!(ExclusiveDisjunctionAny, 0: is_evasive);
        assert_prop!(AllEquivalent, 0: is_evasive);
        assert_prop!(ConjunctionAny, 0: is_evasive);

        assert_prop!(Truth, 0: is_evasive);
    }

    #[test]
    fn test_all_unary_evasive() {
        assert_prop!(Falsity, 1: ! is_evasive);
        assert_prop!(LogicalIdentity: is_evasive);

        assert_prop!(DisjunctionAny, 1: is_evasive);
        assert_prop!(ExclusiveDisjunctionAny, 1: is_evasive);
        assert_prop!(ConjunctionAny, 1: is_evasive);

        assert_prop!(Negation: is_evasive);
        assert_prop!(Truth, 1: ! is_evasive);

        assert_prop!(AllEquivalent, 1: ! is_evasive);
    }

    #[test]
    fn test_all_binary_evasive() {
        assert_prop!(Falsity, 2: ! is_evasive);
        assert_prop!(Conjunction: is_evasive);
        assert_prop!(ConjunctionAny, 2: is_evasive);
        assert_prop!(MaterialNonImplication: is_evasive);
        assert_prop!(First: ! is_evasive);
        assert_prop!(ConverseNonImplication: is_evasive);
        assert_prop!(Last: ! is_evasive);
        assert_prop!(ExclusiveDisjunction: is_evasive);
        assert_prop!(ExclusiveDisjunctionAny, 2: is_evasive);
        assert_prop!(Disjunction: is_evasive);
        assert_prop!(DisjunctionAny, 2: is_evasive);
        assert_prop!(NonDisjunction: is_evasive);
        assert_prop!(LogicalBiconditional: is_evasive);
        assert_prop!(AllEquivalent, 2: is_evasive);
        assert_prop!(NotSecond: ! is_evasive);
        assert_prop!(ConverseImplication: is_evasive);
        assert_prop!(NotFirst: ! is_evasive);
        assert_prop!(MaterialImplication: is_evasive);
        assert_prop!(NonConjunction: is_evasive);
        assert_prop!(Truth, 2: ! is_evasive);
    }

    #[test]
    fn test_all_nullary_fully_evasive() {
        assert_prop!(Falsity, 0: is_fully_evasive);

        assert_prop!(DisjunctionAny, 0: is_fully_evasive);
        assert_prop!(ExclusiveDisjunctionAny, 0: is_fully_evasive);
        assert_prop!(AllEquivalent, 0: is_fully_evasive);
        assert_prop!(ConjunctionAny, 0: is_fully_evasive);

        assert_prop!(Truth, 0: is_fully_evasive);
    }

    #[test]
    fn test_all_unary_fully_evasive() {
        assert_prop!(Falsity, 1: ! is_fully_evasive);
        assert_prop!(LogicalIdentity: is_fully_evasive);

        assert_prop!(DisjunctionAny, 1: is_fully_evasive);
        assert_prop!(ExclusiveDisjunctionAny, 1: is_fully_evasive);
        assert_prop!(ConjunctionAny, 1: is_fully_evasive);

        assert_prop!(Negation: is_fully_evasive);
        assert_prop!(Truth, 1: ! is_fully_evasive);

        assert_prop!(AllEquivalent, 1: ! is_fully_evasive);
    }

    #[test]
    fn test_all_binary_fully_evasive() {
        assert_prop!(Falsity, 2: ! is_fully_evasive);
        assert_prop!(Conjunction: ! is_fully_evasive);
        assert_prop!(ConjunctionAny, 2: ! is_fully_evasive);
        assert_prop!(MaterialNonImplication: ! is_fully_evasive);
        assert_prop!(First: ! is_fully_evasive);
        assert_prop!(ConverseNonImplication: ! is_fully_evasive);
        assert_prop!(Last: ! is_fully_evasive);
        assert_prop!(ExclusiveDisjunction: is_fully_evasive);
        assert_prop!(ExclusiveDisjunctionAny, 2: is_fully_evasive);
        assert_prop!(Disjunction: ! is_fully_evasive);
        assert_prop!(DisjunctionAny, 2: ! is_fully_evasive);
        assert_prop!(NonDisjunction: ! is_fully_evasive);
        assert_prop!(LogicalBiconditional: is_fully_evasive);
        assert_prop!(AllEquivalent, 2: is_fully_evasive);
        assert_prop!(NotSecond: ! is_fully_evasive);
        assert_prop!(ConverseImplication: ! is_fully_evasive);
        assert_prop!(NotFirst: ! is_fully_evasive);
        assert_prop!(MaterialImplication: ! is_fully_evasive);
        assert_prop!(NonConjunction: ! is_fully_evasive);
        assert_prop!(Truth, 2: ! is_fully_evasive);
    }

    #[test]
    fn test_all_nullary_self_dual() {
        assert_prop!(Falsity, 0: ! is_self_dual);

        assert_prop!(DisjunctionAny, 0: ! is_self_dual);
        assert_prop!(ExclusiveDisjunctionAny, 0: ! is_self_dual);
        assert_prop!(AllEquivalent, 0: ! is_self_dual);
        assert_prop!(ConjunctionAny, 0: ! is_self_dual);

        assert_prop!(Truth, 0: ! is_self_dual);
    }

    #[test]
    fn test_all_unary_self_dual() {
        assert_prop!(Falsity, 1: ! is_self_dual);
        assert_prop!(LogicalIdentity: is_self_dual);

        assert_prop!(DisjunctionAny, 1: is_self_dual);
        assert_prop!(ExclusiveDisjunctionAny, 1: is_self_dual);
        assert_prop!(ConjunctionAny, 1: is_self_dual);

        assert_prop!(Negation: is_self_dual);
        assert_prop!(Truth, 1: ! is_self_dual);

        assert_prop!(AllEquivalent, 1: ! is_self_dual);
    }

    #[test]
    fn test_all_binary_self_dual() {
        assert_prop!(Falsity, 2: ! is_self_dual);
        assert_prop!(Conjunction: ! is_self_dual);
        assert_prop!(ConjunctionAny, 2: ! is_self_dual);
        assert_prop!(MaterialNonImplication: ! is_self_dual);
        assert_prop!(First: is_self_dual);
        assert_prop!(ConverseNonImplication: ! is_self_dual);
        assert_prop!(Last: is_self_dual);
        assert_prop!(ExclusiveDisjunction: ! is_self_dual);
        assert_prop!(ExclusiveDisjunctionAny, 2: ! is_self_dual);
        assert_prop!(Disjunction: ! is_self_dual);
        assert_prop!(DisjunctionAny, 2: ! is_self_dual);
        assert_prop!(NonDisjunction: ! is_self_dual);
        assert_prop!(LogicalBiconditional: ! is_self_dual);
        assert_prop!(AllEquivalent, 2: ! is_self_dual);
        assert_prop!(NotSecond: is_self_dual);
        assert_prop!(ConverseImplication: ! is_self_dual);
        assert_prop!(NotFirst: is_self_dual);
        assert_prop!(MaterialImplication: ! is_self_dual);
        assert_prop!(NonConjunction: ! is_self_dual);
        assert_prop!(Truth, 2: ! is_self_dual);
    }

    #[test]
    fn test_all_nullary_sheffer() {
        assert_prop!(Falsity, 0: ! is_sheffer);

        assert_prop!(DisjunctionAny, 0: ! is_sheffer);
        assert_prop!(ExclusiveDisjunctionAny, 0: ! is_sheffer);
        assert_prop!(AllEquivalent, 0: ! is_sheffer);
        assert_prop!(ConjunctionAny, 0: ! is_sheffer);

        assert_prop!(Truth, 0: ! is_sheffer);
    }

    #[test]
    fn test_all_unary_sheffer() {
        assert_prop!(Falsity, 1: ! is_sheffer);
        assert_prop!(LogicalIdentity: ! is_sheffer);

        assert_prop!(DisjunctionAny, 1: ! is_sheffer);
        assert_prop!(ExclusiveDisjunctionAny, 1: ! is_sheffer);
        assert_prop!(ConjunctionAny, 1: ! is_sheffer);

        assert_prop!(Negation: ! is_sheffer);
        assert_prop!(Truth, 1: ! is_sheffer);

        assert_prop!(AllEquivalent, 1: ! is_sheffer);
    }

    #[test]
    fn test_all_binary_sheffer() {
        assert_prop!(Falsity, 2: ! is_sheffer);
        assert_prop!(Conjunction: ! is_sheffer);
        assert_prop!(ConjunctionAny, 2: ! is_sheffer);
        assert_prop!(MaterialNonImplication: ! is_sheffer);
        assert_prop!(First: ! is_sheffer);
        assert_prop!(ConverseNonImplication: ! is_sheffer);
        assert_prop!(Last: ! is_sheffer);
        assert_prop!(ExclusiveDisjunction: ! is_sheffer);
        assert_prop!(ExclusiveDisjunctionAny, 2: ! is_sheffer);
        assert_prop!(Disjunction: ! is_sheffer);
        assert_prop!(DisjunctionAny, 2: ! is_sheffer);
        assert_prop!(NonDisjunction: is_sheffer);
        assert_prop!(LogicalBiconditional: ! is_sheffer);
        assert_prop!(AllEquivalent, 2: ! is_sheffer);
        assert_prop!(NotSecond: ! is_sheffer);
        assert_prop!(ConverseImplication: ! is_sheffer);
        assert_prop!(NotFirst: ! is_sheffer);
        assert_prop!(MaterialImplication: ! is_sheffer);
        assert_prop!(NonConjunction: is_sheffer);
        assert_prop!(Truth, 2: ! is_sheffer);
    }
}

#[cfg(test)]
/// <https://en.wikipedia.org/wiki/Functional_completeness#Minimal_functionally_complete_operator_sets>
mod tests_completeness {
    use crate::connective::BINARY_FUNCTIONS;

    use super::{super::functions::*, *};

    const SHEFFER_FNS: [&dyn BoolFnExt<2>; 2] = [&NonConjunction, &NonDisjunction];

    #[test]
    fn test_only_nand_and_nor_is_sole_complete() {
        let truly_sheffer_ids = SHEFFER_FNS.map(|f| (*f).type_id());

        let mut found_sheffers = 0;
        for &f in BINARY_FUNCTIONS.as_ref() {
            let f: &dyn BoolFnExt<2> = f.up();
            if truly_sheffer_ids.contains(&(*f).type_id()) {
                assert!(f.is_sheffer());
                assert!(is_basis(&[f]));
                found_sheffers += 1;
            } else {
                assert!(!f.is_sheffer());
                assert!(!is_complete(&[f]));
            }
        }

        assert_eq!(found_sheffers, 2);
    }

    type F = &'static dyn BoolFnExt<2>;
    const NF: NotFirst = NotFirst::new();
    const NS: NotSecond = NotSecond::new();
    const PAIR_BASIS: [[F; 2]; 24] = [
        [&Disjunction, &NF],
        [&Disjunction, &NS],
        [&Conjunction, &NF],
        [&Conjunction, &NS],
        [&MaterialImplication, &NF],
        [&MaterialImplication, &NS],
        [&ConverseImplication, &NF],
        [&ConverseImplication, &NS],
        [&MaterialImplication, &Falsity],
        [&ConverseImplication, &Falsity],
        [&MaterialImplication, &ExclusiveDisjunction],
        [&ConverseImplication, &ExclusiveDisjunction],
        [&MaterialImplication, &MaterialNonImplication],
        [&MaterialImplication, &ConverseNonImplication],
        [&ConverseImplication, &MaterialNonImplication],
        [&ConverseImplication, &ConverseNonImplication],
        [&MaterialNonImplication, &NF],
        [&MaterialNonImplication, &NS],
        [&ConverseNonImplication, &NF],
        [&ConverseNonImplication, &NS],
        [&MaterialNonImplication, &Truth],
        [&ConverseNonImplication, &Truth],
        [&MaterialNonImplication, &LogicalBiconditional],
        [&ConverseNonImplication, &LogicalBiconditional],
    ];

    #[test]
    fn two_element_basis() {
        for fs in &PAIR_BASIS {
            assert!(is_complete(fs));
            assert!(is_basis(fs));
        }

        let truly_sheffer_ids = SHEFFER_FNS.map(|f| (*f).type_id());
        dbg!(truly_sheffer_ids);
        let truly_sheffer_pair_ids: Vec<_> = truly_sheffer_ids
            .into_iter()
            .cartesian_product(truly_sheffer_ids)
            .map(<[std::any::TypeId; 2]>::from)
            .collect();

        let pair_basis_type_ids: Vec<_> = PAIR_BASIS
            .iter()
            .flat_map(|&[f1, f2]| {
                // both forward and reverse
                [
                    [(*f1).type_id(), (*f2).type_id()],
                    [(*f2).type_id(), (*f1).type_id()],
                ]
            })
            .collect();

        let all_ids: Vec<_> = BINARY_FUNCTIONS
            .as_ref()
            .iter()
            .map(|f| (**f).type_id())
            .collect();
        dbg!(all_ids);

        let all_pairs: Vec<[&dyn BoolFnExt<2>; 2]> = BINARY_FUNCTIONS
            .as_ref()
            .iter()
            .cartesian_product(BINARY_FUNCTIONS.as_ref())
            .map(|(&f1, &f2)| [f1.up(), f2.up()])
            .collect();
        assert_eq!(all_pairs.len(), 256);

        let mut found_pair_basis = 0;
        let mut found_sheffers = 0;
        let mut found_sheffers_plus = 0;
        let mut found_incomplete = 0;
        for fs in all_pairs {
            let ids = fs.map(|f| (*f).type_id());
            dbg!(ids);
            if pair_basis_type_ids.contains(&ids) {
                // if pair_basis_type_ids
                assert!(is_complete(&fs));
                assert!(is_basis(&fs));
                found_pair_basis += 1;
            } else if truly_sheffer_pair_ids.contains(&ids) {
                assert!(is_complete(&fs));
                assert!(
                    !is_basis(&fs),
                    "Found a pair of identical sheffer function, it is complete but redundant"
                );
                found_sheffers += 1;
            } else if truly_sheffer_ids
                .iter()
                .any(|sheffer| ids.contains(sheffer))
            {
                assert!(is_complete(&fs));
                assert!(
                    !is_basis(&fs),
                    "Found a pair sheffer function plus some more, it is complete but redundant"
                );
                found_sheffers_plus += 1;
            } else {
                assert!(!is_complete(&fs));
                assert!(!is_basis(&fs));
                found_incomplete += 1;
            }
        }

        // both forward and backward
        assert_eq!(found_pair_basis, PAIR_BASIS.len() * 2);

        // (NOR, NOR), (NAND, NAND)
        assert_eq!(found_sheffers, 4);

        // all pairs of (NOR, X), (X, NOR), (NAND, X), (X, NAND) where X not in (NOR, NAND)
        assert_eq!(found_sheffers_plus, 56);

        // all what is left
        assert_eq!(found_incomplete, 256 - 48 - 60);
    }

    const TRIPLE_BASIS: [[F; 3]; 6] = [
        [&Disjunction, &LogicalBiconditional, &Falsity],
        [&Disjunction, &LogicalBiconditional, &ExclusiveDisjunction],
        [&Disjunction, &ExclusiveDisjunction, &Truth],
        [&Conjunction, &LogicalBiconditional, &Falsity],
        [&Conjunction, &LogicalBiconditional, &ExclusiveDisjunction],
        [&Conjunction, &ExclusiveDisjunction, &Truth],
    ];

    #[test]
    fn three_element_basis() {
        for fs in &TRIPLE_BASIS {
            assert!(is_complete(fs));
            assert!(is_basis(fs));
        }

        let truly_sheffer_ids = SHEFFER_FNS.map(|f| (*f).type_id());

        let pair_basis_type_ids: Vec<_> = PAIR_BASIS
            .iter()
            .map(|&[f1, f2]| {
                let mut ids = [(*f1).type_id(), (*f2).type_id()];
                ids.sort();
                ids
            })
            .collect();

        let triple_basis_type_ids: Vec<_> = TRIPLE_BASIS
            .iter()
            .map(|&[f1, f2, f3]| {
                let mut ids = [(*f1).type_id(), (*f2).type_id(), (*f3).type_id()];
                ids.sort();
                ids
            })
            .collect();

        let all_triples: Vec<_> = BINARY_FUNCTIONS
            .as_ref()
            .iter()
            .combinations(3)
            .map(|x| {
                let x: Vec<_> = x.into_iter().copied().map(Upcast::up).collect();
                <[&dyn BoolFnExt<2>; 3]>::try_from(x)
                    .unwrap_or_else(|_| unreachable!("combinations produce Vec-s of size 3"))
            })
            .collect();
        assert_eq!(all_triples.len(), 560); // C(16, 3)

        let mut found_sheffers = 0;
        let mut found_pair_basis = 0;
        let mut found_triple_basis = 0;
        let mut found_incomplete = 0;
        for fs in all_triples {
            let mut ids = fs.map(|f| (*f).type_id());
            ids.sort();

            if truly_sheffer_ids
                .iter()
                .any(|sheffer| ids.contains(sheffer))
            {
                assert!(is_complete(&fs));
                assert!(
                    !is_basis(&fs),
                    "Found a combination with a sheffer function, it is complete but redundant"
                );
                found_sheffers += 1;
            } else if pair_basis_type_ids
                .iter()
                .any(|pt| pt.iter().all(|pair_id| ids.contains(pair_id)))
            {
                assert!(is_complete(&fs));
                assert!(
                    !is_basis(&fs),
                    "Found a pair basis, it is complete but not minimal"
                );
                found_pair_basis += 1;
            } else if triple_basis_type_ids.contains(&ids) {
                assert!(is_complete(&fs));
                assert!(
                    is_basis(&fs),
                    "Found a triple basis, it is complete and minimal"
                );
                found_triple_basis += 1;
            } else {
                assert!(!is_complete(&fs));
                assert!(!is_basis(&fs), "Found incomplete");
                found_incomplete += 1;
            }
        }

        // twice C(15, 2) minus duplicates where NAND and NOR appeared both (14)
        assert_eq!(found_sheffers, 2 * (15 * 14 / 2) - 14);

        // 24 * (every possible of 16 - 2(shefer) - 2(pair itself)) = 24 * 12 = 288
        // ???
        assert_eq!(found_pair_basis, 200);
        assert_eq!(found_triple_basis, TRIPLE_BASIS.len());

        // all what is left
        assert_eq!(found_incomplete, 560 - 196 - 200 - 6);
    }

    #[test]
    fn no_basis_in_quadruples() {
        let truly_sheffer_ids = SHEFFER_FNS.map(|f| (*f).type_id());

        let pair_basis_type_ids: Vec<_> = PAIR_BASIS
            .iter()
            .map(|&[f1, f2]| {
                let mut ids = [(*f1).type_id(), (*f2).type_id()];
                ids.sort();
                ids
            })
            .collect();

        let triple_basis_type_ids: Vec<_> = TRIPLE_BASIS
            .iter()
            .map(|&[f1, f2, f3]| {
                let mut ids = [(*f1).type_id(), (*f2).type_id(), (*f3).type_id()];
                ids.sort();
                ids
            })
            .collect();

        let all_quadruples: Vec<_> = BINARY_FUNCTIONS
            .as_ref()
            .iter()
            .combinations(4)
            .map(|x| {
                let x: Vec<_> = x.into_iter().copied().map(Upcast::up).collect();
                <[&dyn BoolFnExt<2>; 4]>::try_from(x)
                    .unwrap_or_else(|_| unreachable!("combinations produce Vec-s of size 3"))
            })
            .collect();
        assert_eq!(all_quadruples.len(), 1820); // C(16, 4)

        for fs in all_quadruples {
            assert!(!is_basis(&fs));
            let mut ids = fs.map(|f| (*f).type_id());
            ids.sort();

            let is_sheffer = || {
                truly_sheffer_ids
                    .iter()
                    .any(|sheffer| ids.contains(sheffer))
            };

            let is_pair_basis = || {
                pair_basis_type_ids
                    .iter()
                    .any(|pt| pt.iter().all(|pair_id| ids.contains(pair_id)))
            };

            let is_triple_basis = || {
                triple_basis_type_ids
                    .iter()
                    .any(|pt| pt.iter().all(|pair_id| ids.contains(pair_id)))
            };

            if is_sheffer() || is_pair_basis() || is_triple_basis() {
                assert!(is_complete(&fs));
            } else {
                assert!(!is_complete(&fs));
            }
        }
    }
}
