//! A [truth table](https://en.wikipedia.org/wiki/Truth_table)
//! is a mathematical table which sets out the functional values
//! of logical expressions on each of their functional arguments,
//! that is, for each combination of values taken by their logical variables.
//!
//! A truth table has one column for each input variable (for example, A and B),
//! and one final column showing all of the possible results of the logical operation
//! that the table represents (for example, A XOR B).
//! Each row of the truth table contains one possible configuration of the input variables
//! (for instance, A=true, B=false), and the result of the operation for those values.
use std::{collections::BTreeMap, iter};

use itertools::Itertools as _;

use super::TruthFunction;

/// Generate a [truth table](https://en.wikipedia.org/wiki/Truth_table)
/// for an [`TruthFunction`]
/// with the values produced by applying
/// the arguments in default order
/// (as the sequence of incrementing binary numbers):
///
/// # Example
///
/// For the XOR-function the last column will be produced.
///
/// ```text
/// |  i |    args | value |
/// |----|---------|-------|
/// |  0 |  (0, 0) |     0 |
/// |  1 |  (0, 1) |     1 |
/// |  2 |  (1, 0) |     1 |
/// |  3 |  (1, 1) |     0 |
/// ```
pub fn get<Op, const ARITY: usize>() -> Vec<bool>
where
    Op: TruthFunction<ARITY>,
{
    get_mapping::<Op, ARITY>().into_values().collect()
}

/// Generate a [truth table](https://en.wikipedia.org/wiki/Truth_table)
/// for an [`TruthFunction`] as the **key** (boolean arguments)-**value** (function result)
/// ordered map.
///
/// # Panics
///
/// If a single point of cartesian product of `ARITY` bool values
/// does not contain exactly `ARITY` values.
/// This invariant should be guaranteed by the
/// [itertools library][itertools::Itertools::multi_cartesian_product].
pub fn get_mapping<Op, const ARITY: usize>() -> BTreeMap<[bool; ARITY], bool>
where
    Op: TruthFunction<ARITY>,
{
    let table: BTreeMap<_, _> = iter::repeat([false, true])
        .take(ARITY)
        .multi_cartesian_product()
        .map(|assignment| {
            let assignment = assignment.try_into().unwrap();
            (assignment, Op::init().eval(assignment))
        })
        .collect();

    if ARITY > 0 {
        assert_eq!(table.len(), 1 << ARITY);
        table
    } else {
        assert!(table.is_empty());
        assert_eq!(ARITY, 0);
        let dummy_empty_array = [false; ARITY];
        iter::once((dummy_empty_array, Op::init().eval(dummy_empty_array))).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::{super::*, *};

    #[test]
    fn nullary_truth() {
        type T = Truth;
        let table = get::<T, 0>();
        assert_eq!(table, [true]);

        let table = get_mapping::<T, 0>();
        assert_eq!(table.into_iter().collect_vec(), vec![([], true)]);
    }

    #[test]
    fn nullary_false() {
        type T = Falsity;
        let table = get::<T, 0>();
        assert_eq!(table, [false]);

        let table = get_mapping::<T, 0>();
        assert_eq!(table.into_iter().collect_vec(), vec![([], false)]);
    }

    #[test]
    fn unary_truth() {
        type T = Truth;
        let table = get::<T, 1>();
        assert_eq!(table, [true, true]);

        let table = get_mapping::<T, 1>();
        assert_eq!(
            table.into_iter().collect_vec(),
            vec![([false], true), ([true], true)]
        );
    }

    #[test]
    fn unary_false() {
        type T = Falsity;
        let table = get::<T, 1>();
        assert_eq!(table, [false, false]);

        let table = get_mapping::<T, 1>();
        assert_eq!(
            table.into_iter().collect_vec(),
            vec![([false], false), ([true], false)]
        );
    }

    #[test]
    fn binary_truth() {
        type T = Truth;
        let table = get::<T, 2>();
        assert_eq!(table, [true; 4]);

        let table = get_mapping::<T, 2>();
        assert_eq!(
            table.into_iter().collect_vec(),
            vec![
                ([false, false], true),
                ([false, true], true),
                ([true, false], true),
                ([true, true], true),
            ]
        );
    }

    #[test]
    fn binary_false() {
        type T = Falsity;
        let table = get::<T, 2>();
        assert_eq!(table, [false; 4]);

        let table = get_mapping::<T, 2>();
        assert_eq!(
            table.into_iter().collect_vec(),
            vec![
                ([false, false], false),
                ([false, true], false),
                ([true, false], false),
                ([true, true], false),
            ]
        );
    }

    #[test]
    fn unary_identity() {
        type T = LogicalIdentity;
        let table = get::<T, 1>();
        assert_eq!(table, [false, true]);

        let table = get_mapping::<T, 1>();
        assert_eq!(
            table.into_iter().collect_vec(),
            vec![([false], false), ([true], true)]
        );
    }

    #[test]
    fn unary_negation() {
        type T = Negation;
        let table = get::<T, 1>();
        assert_eq!(table, [true, false]);

        let table = get_mapping::<T, 1>();
        assert_eq!(
            table.into_iter().collect_vec(),
            vec![([false], true), ([true], false)]
        );
    }

    #[test]
    fn left_projection() {
        type T = Projection<0>;
        let table = get::<T, 2>();
        assert_eq!(table, [false, false, true, true]);

        let table = get_mapping::<T, 2>();
        assert_eq!(
            table.into_iter().collect_vec(),
            vec![
                ([false, false], false),
                ([false, true], false),
                ([true, false], true),
                ([true, true], true),
            ]
        );
    }

    #[test]
    fn right_projection() {
        type T = Projection<1>;
        let table = get::<T, 2>();
        assert_eq!(table, [false, true, false, true]);

        let table = get_mapping::<T, 2>();
        assert_eq!(
            table.into_iter().collect_vec(),
            vec![
                ([false, false], false),
                ([false, true], true),
                ([true, false], false),
                ([true, true], true),
            ]
        );
    }

    #[test]
    fn neg_left_projection() {
        type T = ProjectAndUnary<0, Negation>;
        let table = get::<T, 2>();
        assert_eq!(table, [true, true, false, false]);

        let table = get_mapping::<T, 2>();
        assert_eq!(
            table.into_iter().collect_vec(),
            vec![
                ([false, false], true),
                ([false, true], true),
                ([true, false], false),
                ([true, true], false),
            ]
        );
    }

    #[test]
    fn neg_right_projection() {
        type T = ProjectAndUnary<1, Negation>;
        let table = get::<T, 2>();
        assert_eq!(table, [true, false, true, false]);

        let table = get_mapping::<T, 2>();
        assert_eq!(
            table.into_iter().collect_vec(),
            vec![
                ([false, false], true),
                ([false, true], false),
                ([true, false], true),
                ([true, true], false),
            ]
        );
    }
}
