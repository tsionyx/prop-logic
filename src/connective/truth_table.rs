//! Define [truth table](https://en.wikipedia.org/wiki/Truth_table)
//! for [`BoolFn`]-s.
use std::fmt;

use crate::{
    arity::TwoPower,
    connective::BoolFn,
    truth_table::{TruthTable, TruthTabled},
    utils::dependent_array::{ArrayLength, CheckedStorage, GenericArray, SizeMapper},
};

impl<const ARITY: usize, F> TruthTabled<ARITY> for F
where
    F: BoolFn<ARITY>,
    TwoPower: SizeMapper<ARITY, ArrSize: ArrayLength>,
{
    type TT = FixedTruthTable<ARITY>;

    fn get_truth_table(&self) -> Self::TT {
        bool_truth_table(self)
    }
}

fn bool_truth_table<const ARITY: usize, F>(f: &F) -> FixedTruthTable<ARITY>
where
    F: BoolFn<ARITY>,
    TwoPower: SizeMapper<ARITY, ArrSize: ArrayLength>,
{
    use itertools::Itertools as _;
    use std::collections::BTreeMap as Map;

    #[expect(clippy::manual_repeat_n)]
    let table: Map<_, _> = std::iter::repeat([false, true])
        .take(ARITY)
        .multi_cartesian_product()
        .map(|assignment| {
            let assignment = assignment
                .try_into()
                .expect("The array size is guaranteed by Itertools::multi_cartesian_product");
            (assignment, f.compose(assignment))
        })
        .collect();

    let table = if ARITY == 0 {
        assert!(table.is_empty());
        let dummy_empty_array = [false; ARITY];
        let row: Row<ARITY> = (dummy_empty_array, f.compose(dummy_empty_array));
        vec![row]
    } else {
        table.into_iter().collect()
    };

    assert_eq!(table.len(), 1 << ARITY);
    table
        .try_into()
        .map_or_else(|_| panic!("Size checked before"), FixedTruthTable::new)
}

impl<const ARITY: usize> TruthTable<ARITY> for FixedTruthTable<ARITY>
where
    TwoPower: SizeMapper<ARITY, ArrSize: ArrayLength>,
{
    type Input = [bool; ARITY];

    fn iter(&self) -> impl Iterator<Item = &(Self::Input, bool)> {
        use crate::utils::dependent_array::SizedArray;

        self.table.as_ref().iter()
    }

    fn into_inner(self) -> Self::Repr {
        self.table.into_inner()
    }
}

/// A [truth table](https://en.wikipedia.org/wiki/Truth_table)
/// for arbitrary [`BoolFn`][super::BoolFn]
///
/// with the values produced by applying
/// the arguments in default order
/// (as the sequence of incrementing binary numbers):
///
/// # Example
///
/// For the XOR-function the last two column will be produced.
///
/// ```text
/// |  i |    args | value |
/// |----|---------|-------|
/// |  0 |  (0, 0) |     0 |
/// |  1 |  (0, 1) |     1 |
/// |  2 |  (1, 0) |     1 |
/// |  3 |  (1, 1) |     0 |
/// ```
pub struct FixedTruthTable<const ARITY: usize>
where
    TwoPower: SizeMapper<ARITY, ArrSize: ArrayLength>,
{
    table: CheckedStorage<Row<ARITY>, ARITY, TwoPower>,
}

impl<const ARITY: usize> fmt::Debug for FixedTruthTable<ARITY>
where
    TwoPower: SizeMapper<ARITY, ArrSize: ArrayLength>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TruthTable")
            .field("table", self.table.as_ref())
            .finish()
    }
}

type Row<const ARITY: usize> = ([bool; ARITY], bool);

impl<const ARITY: usize> FixedTruthTable<ARITY>
where
    TwoPower: SizeMapper<ARITY, ArrSize: ArrayLength>,
{
    // /// Create new [`FixedTruthTable`] from the individual rows.
    // const fn new(table: <D as CheckedArray<ARITY>>::Array<Row<ARITY>>) -> Self {
    //     Self {
    //         table: CheckedStorage::new(table),
    //     }
    // }

    /// Return inner [array][CheckedArray::Array].
    fn into_inner(self) -> GenericArray<Row<ARITY>, <TwoPower as SizeMapper<ARITY>>::ArrSize> {
        self.table.0
    }
}

impl<const ARITY: usize> IntoIterator for FixedTruthTable<ARITY>
where
    TwoPower: SizeMapper<ARITY, ArrSize: ArrayLength>,
{
    type Item = Row<ARITY>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        Vec::new().into_iter();
        self.into_inner().into_iter()
    }
}

impl<const ARITY: usize> fmt::Display for FixedTruthTable<ARITY>
where
    TwoPower: SizeMapper<ARITY, ArrSize: ArrayLength>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value_col = "VALUE";
        let padding_value = value_col.len();
        let padding_var = value_col.len() - 1;
        let padding_sep = value_col.len() + 2;

        for i in 0..ARITY {
            write!(f, "| x{i:<padding_var$} ")?;
        }
        writeln!(f, "| {value_col} |")?;

        for _ in 0..=ARITY {
            write!(f, "|{:-<padding_sep$}", "")?;
        }
        write!(f, "|")?;

        let rows = self.table.clone().into_inner();
        for (args, res) in rows {
            writeln!(f)?;
            for arg in args {
                write!(f, "| {arg:<padding_value$} ")?;
            }
            write!(f, "| {res:<padding_value$} |")?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{super::*, *};

    fn get<Op, const ARITY: usize>() -> Vec<bool>
    where
        Op: BoolFn<ARITY> + InitFn,
        TwoPower: SizeMapper<ARITY>,
    {
        get_mapping::<Op, ARITY>()
            .into_iter()
            .map(|(_, v)| v)
            .collect()
    }

    fn get_mapping<Op, const ARITY: usize>() -> Vec<Row<ARITY>>
    where
        Op: BoolFn<ARITY> + InitFn,
        TwoPower: SizeMapper<ARITY>,
    {
        let table = Op::init().get_truth_table();
        table.table.into_inner().into()
    }

    #[test]
    fn nullary_truth() {
        type T = Truth;
        let table = get::<T, 0>();
        assert_eq!(table, [true]);
        assert_eq!(table, get::<ConjunctionAny, 0>());
        assert_eq!(table, get::<AllEquivalent, 0>());

        let table = get_mapping::<T, 0>();
        assert_eq!(table, vec![([], true)]);
        assert_eq!(table, get_mapping::<ConjunctionAny, 0>());
        assert_eq!(table, get_mapping::<AllEquivalent, 0>());
    }

    #[test]
    fn nullary_false() {
        type T = Falsity;
        let table = get::<T, 0>();
        assert_eq!(table, [false]);
        assert_eq!(table, get::<DisjunctionAny, 0>());
        assert_eq!(table, get::<ExclusiveDisjunctionAny, 0>());

        let table = get_mapping::<T, 0>();
        assert_eq!(table, vec![([], false)]);
        assert_eq!(table, get_mapping::<DisjunctionAny, 0>());
        assert_eq!(table, get_mapping::<ExclusiveDisjunctionAny, 0>());
    }

    #[test]
    fn unary_truth() {
        type T = Truth;
        let table = get::<T, 1>();
        assert_eq!(table, [true, true]);
        assert_eq!(table, get::<AllEquivalent, 1>());

        let table = get_mapping::<T, 1>();
        assert_eq!(table, vec![([false], true), ([true], true)]);
        assert_eq!(table, get_mapping::<AllEquivalent, 1>());
    }

    #[test]
    fn unary_false() {
        type T = Falsity;
        let table = get::<T, 1>();
        assert_eq!(table, [false, false]);

        let table = get_mapping::<T, 1>();
        assert_eq!(table, vec![([false], false), ([true], false)]);
    }

    #[test]
    fn binary_truth() {
        type T = Truth;
        let table = get::<T, 2>();
        assert_eq!(table, [true; 4]);

        let table = get_mapping::<T, 2>();
        assert_eq!(
            table,
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
            table,
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
        assert_eq!(table, get::<ConjunctionAny, 1>());
        assert_eq!(table, get::<DisjunctionAny, 1>());
        assert_eq!(table, get::<ExclusiveDisjunctionAny, 1>());

        let table = get_mapping::<T, 1>();
        assert_eq!(table, vec![([false], false), ([true], true)]);
        assert_eq!(table, get_mapping::<ConjunctionAny, 1>());
        assert_eq!(table, get_mapping::<DisjunctionAny, 1>());
        assert_eq!(table, get_mapping::<ExclusiveDisjunctionAny, 1>());
    }

    #[test]
    fn unary_negation() {
        type T = Negation;
        let table = get::<T, 1>();
        assert_eq!(table, [true, false]);

        let table = get_mapping::<T, 1>();
        assert_eq!(table, vec![([false], true), ([true], false)]);
    }

    #[test]
    fn left_projection() {
        type T = First;
        let table = get::<T, 2>();
        assert_eq!(table, [false, false, true, true]);

        let table = get_mapping::<T, 2>();
        assert_eq!(
            table,
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
        type T = Last;
        let table = get::<T, 2>();
        assert_eq!(table, [false, true, false, true]);

        let table = get_mapping::<T, 2>();
        assert_eq!(
            table,
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
        type T = NotFirst;
        let table = get::<T, 2>();
        assert_eq!(table, [true, true, false, false]);

        let table = get_mapping::<T, 2>();
        assert_eq!(
            table,
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
        type T = NotSecond;
        let table = get::<T, 2>();
        assert_eq!(table, [true, false, true, false]);

        let table = get_mapping::<T, 2>();
        assert_eq!(
            table,
            vec![
                ([false, false], true),
                ([false, true], false),
                ([true, false], true),
                ([true, true], false),
            ]
        );
    }

    #[test]
    fn binary_conjunction() {
        type T = Conjunction;
        let table = get::<T, 2>();
        assert_eq!(table, [false, false, false, true]);
        assert_eq!(table, get::<ConjunctionAny, 2>());

        let table = get_mapping::<T, 2>();
        assert_eq!(
            table,
            vec![
                ([false, false], false),
                ([false, true], false),
                ([true, false], false),
                ([true, true], true),
            ]
        );
        assert_eq!(table, get_mapping::<ConjunctionAny, 2>());
    }

    #[test]
    fn binary_disjunction() {
        type T = Disjunction;
        let table = get::<T, 2>();
        assert_eq!(table, [false, true, true, true]);
        assert_eq!(table, get::<DisjunctionAny, 2>());

        let table = get_mapping::<T, 2>();
        assert_eq!(
            table,
            vec![
                ([false, false], false),
                ([false, true], true),
                ([true, false], true),
                ([true, true], true),
            ]
        );
        assert_eq!(table, get_mapping::<DisjunctionAny, 2>());
    }

    #[test]
    fn binary_xor() {
        type T = ExclusiveDisjunction;
        let table = get::<T, 2>();
        assert_eq!(table, [false, true, true, false]);
        assert_eq!(table, get::<ExclusiveDisjunctionAny, 2>());

        let table = get_mapping::<T, 2>();
        assert_eq!(
            table,
            vec![
                ([false, false], false),
                ([false, true], true),
                ([true, false], true),
                ([true, true], false),
            ]
        );
        assert_eq!(table, get_mapping::<ExclusiveDisjunctionAny, 2>());
    }

    #[test]
    fn binary_eq() {
        type T = LogicalBiconditional;
        let table = get::<T, 2>();
        assert_eq!(table, [true, false, false, true]);
        assert_eq!(table, get::<AllEquivalent, 2>());

        let table = get_mapping::<T, 2>();
        assert_eq!(
            table,
            vec![
                ([false, false], true),
                ([false, true], false),
                ([true, false], false),
                ([true, true], true),
            ]
        );
        assert_eq!(table, get_mapping::<AllEquivalent, 2>());
    }
}
