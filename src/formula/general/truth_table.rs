//! Define [truth table](https://en.wikipedia.org/wiki/Truth_table)
//! for [`Formula`]-s.
use std::{fmt, hash::Hash};

use thiserror::Error;

use crate::{
    formula::Valuation,
    truth_table::{TruthTable, TruthTabled},
    utils::vec::UnsortedVec,
};

use super::formula::Formula;

impl<T> TruthTabled for Formula<T>
where
    T: Clone + Eq + Hash,
{
    type TT = FormulaTruthTable<T>;

    fn get_truth_table(&self) -> Self::TT {
        use itertools::Itertools as _;

        let atoms = self.atoms();
        let arity = atoms.len();

        #[expect(clippy::manual_repeat_n)]
        let table: Vec<_> = if arity == 0 {
            let dummy_assignment = vec![];
            let valuation = Valuation::<T>::empty();
            let row = if let Self::TruthValue(value) = self.interpret(&valuation) {
                (dummy_assignment, value)
            } else {
                panic!("Cannot evaluate formala with no atoms")
            };
            vec![row]
        } else {
            std::iter::repeat([false, true])
                .take(arity)
                .multi_cartesian_product()
                .filter_map(|assignment| {
                    assert_eq!(
                        assignment.len(),
                        arity,
                        "The array size is guaranteed by Itertools::multi_cartesian_product"
                    );
                    let valuation: Valuation<_> = atoms
                        .iter()
                        .copied()
                        .cloned()
                        .zip(assignment.iter().copied())
                        .collect();
                    if let Self::TruthValue(value) = self.interpret(&valuation) {
                        Some((assignment, value))
                    } else {
                        None
                    }
                })
                .collect()
        };

        assert_eq!(table.len(), 1 << arity, "The table is complete");
        let atoms = atoms.into_iter().cloned().collect();
        FormulaTruthTable { atoms, table }
    }

    fn is_equivalent<Rhs>(&self, other: &Rhs) -> bool
    where
        Rhs: TruthTabled<0, TT = Self::TT>,
    {
        self.get_truth_table()
            .is_equivalent(&other.get_truth_table())
    }
}

type LazyRow<'a, T> = Box<dyn Iterator<Item = (&'a T, bool)> + 'a>;

impl<T> TruthTable for FormulaTruthTable<T> {
    type Row<'a>
        = LazyRow<'a, T>
    where
        T: 'a;

    fn iter(&self) -> impl Iterator<Item = (Self::Row<'_>, bool)> {
        fn swap<T, U>((a, b): (T, U)) -> (U, T) {
            (b, a)
        }
        self.table.iter().map(|(args, res)| {
            let args: Self::Row<'_> = Box::new(args.iter().copied().zip(&self.atoms).map(swap));
            (args, *res)
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// A [truth table](https://en.wikipedia.org/wiki/Truth_table)
/// for an arbitrary [`Formula`]
///
/// with the values produced by assigning values to
/// the [`Formula`]'s atoms in default order
/// (as the sequence of incrementing binary numbers).
pub struct FormulaTruthTable<T> {
    atoms: Vec<T>,
    table: Table,
}

type Table = Vec<(Vec<bool>, bool)>;

impl<T> FormulaTruthTable<T>
where
    T: PartialEq,
{
    /// Check if the two truth tables are equivalent in respect to:
    /// - the order of the variables;
    /// - the presense/absence of the redundant variables
    ///   (the ones having no effect on the result).
    ///
    /// For example, consider the following three truth tables:
    ///
    /// ```text
    /// | p | q | value |
    /// |---|---|-------|
    /// | 0 | 0 |     1 |
    /// | 0 | 1 |     1 |
    /// | 1 | 0 |     0 |
    /// | 1 | 1 |     0 |
    /// ```
    ///
    /// ```text
    /// | q | p | value |
    /// |---|---|-------|
    /// | 0 | 0 |     1 |
    /// | 0 | 1 |     0 |
    /// | 1 | 0 |     1 |
    /// | 1 | 1 |     0 |
    /// ```
    ///
    /// ```text
    /// | p | value |
    /// |---|-------|
    /// | 0 |     1 |
    /// | 1 |     0 |
    /// ```
    ///
    /// All the formulae can easily be reduced to `¬p`, but:
    /// - the first formula has the order of variables `['p', 'q']`;
    /// - the second formula has the order of variables `['q', 'p']`;
    /// - the last formula has only a single variable `p`;
    ///
    /// However, all of them are equivalent to each other.
    ///
    /// <https://en.wikipedia.org/wiki/Logical_equivalence>
    pub fn is_equivalent(&self, other: &Self) -> bool {
        if self == other {
            return true;
        }

        let a = self.reduce().unwrap_or_else(|_| self.as_ref());
        let b = other.reduce().unwrap_or_else(|_| other.as_ref());

        let a_atoms: UnsortedVec<_> = a.atoms.iter().collect();
        let b_atoms: UnsortedVec<_> = b.atoms.iter().collect();
        if a_atoms != b_atoms {
            return false;
        }

        // The following piece tries to compare the rows even if they are mixed up.
        //
        // The complexity of a single row-to-row comparison is `O(n²)`
        // where `n` is the number of atoms in the formula.
        // Having `n` atoms we have `2ⁿ` total rows. ([U+207F](https://unicodeplus.com/U+207F) can be entered with `Ctrl-Shift-U + 2 + 0 + 7 + f`).
        // Comparing that many rows with each other requires `O(2²ⁿ)` row comparisons
        // or `O(2²ⁿ * n²)` atomic comparisons in total.
        //
        // That gets pretty soon out of control as the number of atoms grows:
        //
        // | atoms | # comparisons |
        // |-------|---------------|
        // |     1 |             4 |
        // |     2 |            64 |
        // |     3 |           576 |
        // |     4 |         4_096 |
        // |     5 |        25_600 |
        // |     6 |       147_456 |
        // |     7 |       802_816 |
        // |     8 |     4_194_304 |
        // |     9 |    21_233_664 |
        // |    10 |   104_857_600 |
        // |    11 |   507_510_784 |
        // |    12 | 2_415_919_104 |

        let a_rows: UnsortedVec<_> = a
            .iter()
            .map(|(row, evaluated)| (UnsortedVec(row.collect()), evaluated))
            .collect();
        let b_rows: UnsortedVec<_> = b
            .iter()
            .map(|(row, evaluated)| (UnsortedVec(row.collect()), evaluated))
            .collect();
        a_rows == b_rows
    }

    /// Remove the non-significant atoms (by shrinking the table twice for each of it).
    ///
    /// - select a candidate atom to remove (the column in a table);
    /// - split the table into two sorted sides by taking the next unprocessed row and placing its counterpart in the other side;
    /// - if the values column of the two sides are equal, then the candidate atom is non-significant and can be removed;
    fn reduce(&self) -> Result<FormulaTruthTable<&T>, ReduceError<&T>> {
        let mut current = self.as_ref();

        let mut atoms_to_remove = Vec::new();
        // Iterate over the atoms and try to remove them one by one.
        for atom in &self.atoms {
            let is_significant = current.reduce_atom(&atom)?;

            let table = match is_significant {
                Significance::Significant { a, b } => {
                    // If the atom is significant, we keep it and continue with the next one.
                    a.into_iter().chain(b).collect()
                }
                Significance::NonSignificant { atom_idx, side } => {
                    // If the atom is not significant, we remove it.
                    atoms_to_remove.push(atom);
                    side.into_iter()
                        .map(|(mut row, value)| {
                            let _ = row.remove(atom_idx);
                            (row, value)
                        })
                        .collect()
                }
            };

            let atoms = self
                .atoms
                .iter()
                .filter(|atom| !atoms_to_remove.contains(atom))
                .collect();

            current = FormulaTruthTable { atoms, table }
        }

        Ok(current)
    }

    fn reduce_atom(self, atom: &T) -> Result<Significance<Table>, ReduceError<&T>> {
        let Self { atoms, mut table } = self;
        let atom_idx = atoms.iter().position(|a| a == atom).ok_or(ReduceError {
            reason: ReduceErrorReason::AtomNotFound,
            atom,
        })?;

        let (mut a_side, mut b_side) = (Vec::new(), Vec::new());

        while let Some(row_with_value) = table.pop() {
            let row = &row_with_value.0;
            let atom_value = row[atom_idx];

            // and its counterpart (the same row with the atom value flipped) to the `b_side`.
            let counter_part_row_idx = table.iter().map(|(row, _)| row).position(|other_row| {
                // Find the row with the same values except for the atom at `atom_idx`.
                row.iter()
                    .zip(other_row)
                    .enumerate()
                    .all(|(i, (v, u))| (v == u) || (i == atom_idx))
            });
            if let Some(counter_part_row_idx) = counter_part_row_idx {
                let counter_part = table.remove(counter_part_row_idx);
                if counter_part.0[atom_idx] == atom_value {
                    return Err(ReduceError {
                        reason: ReduceErrorReason::CounterRowInvalid,
                        atom,
                    });
                }
                a_side.push(row_with_value);
                b_side.push(counter_part);
            } else {
                return Err(ReduceError {
                    reason: ReduceErrorReason::CounterRowNotFound {
                        row: row_with_value.clone(),
                    },
                    atom,
                });
            }
        }

        debug_assert_eq!(
            a_side.len(),
            b_side.len(),
            "The sides should have the same length"
        );
        debug_assert!(
            table.is_empty(),
            "The table should be empty after processing"
        );

        let is_significant = a_side
            .iter()
            .zip(&b_side)
            .any(|((_, a_value), (_, b_value))| {
                // Check if the values of the two sides are equal.
                a_value != b_value
            });

        Ok(if is_significant {
            Significance::Significant {
                a: a_side,
                b: b_side,
            }
        } else {
            Significance::NonSignificant {
                atom_idx,
                side: a_side,
            }
        })
    }

    fn as_ref(&self) -> FormulaTruthTable<&T> {
        FormulaTruthTable {
            atoms: self.atoms.iter().collect(),
            table: self.table.clone(),
        }
    }
}

#[derive(Debug, Error)]
#[error("Reduce failed: {reason}")]
struct ReduceError<T> {
    reason: ReduceErrorReason,
    atom: T,
}

impl<T> From<ReduceError<&T>> for ReduceError<T>
where
    T: Clone,
{
    fn from(value: ReduceError<&T>) -> Self {
        let ReduceError { reason, atom } = value;
        Self {
            reason,
            atom: atom.clone(),
        }
    }
}

#[derive(Debug, Clone, Error)]
enum ReduceErrorReason {
    #[error("Atom not found in the truth table")]
    AtomNotFound,
    #[error("No counterpart row found for row {row:?}")]
    CounterRowNotFound { row: (Vec<bool>, bool) },
    #[error("Counterpart row should have the opposite value")]
    CounterRowInvalid,
}

enum Significance<T> {
    Significant { a: T, b: T },
    NonSignificant { atom_idx: usize, side: T },
}

impl<T> fmt::Display for FormulaTruthTable<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let atoms: Vec<_> = self
            .iter()
            .next()
            .map(|(args, _)| args.map(|(atom, _)| atom).collect())
            .unwrap_or_default();
        let value_col = "VALUE";
        let padding_value = value_col.len();
        let padding_value = atoms
            .iter()
            .map(|a| a.to_string().len())
            .max()
            .unwrap_or(padding_value)
            .max(padding_value);
        let padding_var = padding_value;
        let padding_sep = padding_value + 2;

        for atom in &atoms {
            // FIXME: {atom:<padding_var$} is not working
            write!(f, "| {:<padding_var$} ", atom.to_string())?;
        }
        writeln!(f, "| {value_col} |")?;

        let arity = atoms.len();
        for _ in 0..=arity {
            write!(f, "|{:-<padding_sep$}", "")?;
        }
        write!(f, "|")?;

        for (args, res) in self.iter() {
            writeln!(f)?;
            for (_atom, arg) in args {
                write!(f, "| {arg:<padding_value$} ")?;
            }
            write!(f, "| {res:<padding_value$} |")?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{super::super::Variable, *};

    #[test]
    fn three_vars_formula() {
        let p = Variable::with_data(1, "p");
        let q = Variable::with_data(2, "q");
        let r = Variable::with_data(3, "r");
        let f = (Formula::from(p) | !q) & (Formula::from(q) ^ !r);

        let truth_table = f.get_truth_table();
        println!("{f}");
        println!("{truth_table}");
        // let expected = FormulaTruthTable {
        //     columns: vec![
        //         (
        //             Arc::new(p),
        //             vec![false, false, false, false, true, true, true, true],
        //         ),
        //         (
        //             Arc::new(q),
        //             vec![false, false, true, true, false, false, true, true],
        //         ),
        //         (
        //             Arc::new(r),
        //             vec![false, true, false, true, false, true, false, true],
        //         ),
        //     ],
        //     values: vec![true, false, false, false, true, false, false, true],
        // };
        // assert_eq!(truth_table, expected, "Truth table does not match expected");

        let rows: Vec<(Vec<_>, bool)> = truth_table
            .iter()
            .map(|(row, value)| (row.map(|(_, val)| val).collect(), value))
            .collect();
        let expected = vec![
            (vec![false, false, false], true),
            (vec![false, false, true], false),
            (vec![false, true, false], false),
            (vec![false, true, true], false),
            (vec![true, false, false], true),
            (vec![true, false, true], false),
            (vec![true, true, false], false),
            (vec![true, true, true], true),
        ];
        assert_eq!(
            rows, expected,
            "Rows of the truth table do not match expected"
        );

        for (row, _val) in truth_table.iter() {
            let atoms: Vec<_> = row.map(|(&atom, _)| atom).collect();
            assert_eq!(atoms, vec![p, q, r]);
        }
    }

    #[test]
    fn commutative_equivalence() {
        let p = Variable::with_data(1, 'p');
        let q = Variable::with_data(2, 'q');

        let f1 = Formula::atom(p) & q;
        let f2 = Formula::atom(q) & p;

        assert!(f1.is_equivalent(&f2), "{f1} != {f2}");
        assert!(f2.is_equivalent(&f1), "{f2} != {f1}");
    }

    #[test]
    fn large_conjunction_equivalence() {
        const ATOMS: u8 = 10;

        let vars: Vec<_> = (0..ATOMS)
            .map(|i| Variable::with_data(i.into(), (b'a' + i) as char))
            .collect();

        let f_straight = vars
            .iter()
            .copied()
            .map(Formula::atom)
            .reduce(|acc, x| acc & x)
            .unwrap();

        let f_reverse = vars
            .iter()
            .copied()
            .map(Formula::atom)
            .reduce(|acc, x| x & acc)
            .unwrap();

        assert_ne!(f_straight, f_reverse);
        assert!(
            f_straight.is_equivalent(&f_reverse),
            "{f_straight} != {f_reverse}"
        );
    }

    #[test]
    fn absorption_equivalence() {
        let p = Variable::with_data(1, 'p');
        let q = Variable::with_data(2, 'q');

        let f1 = Formula::atom(p);
        let f2 = (Formula::atom(p) | q) & p;

        // (p ∨ q) ∧ p ≡ p
        assert!(f1.is_equivalent(&f2), "{f1} != {f2}");
        assert!(f2.is_equivalent(&f1), "{f2} != {f1}");
    }

    #[test]
    fn absorption_equivalence_2() {
        let p = Variable::with_data(1, 'p');
        let q = Variable::with_data(2, 'q');

        let f1 = Formula::atom(p);
        let f2 = (Formula::atom(q) & p) | p;

        println!("{f1}");
        println!("{}", f1.get_truth_table());

        println!("{f2}");
        println!("{}", f2.get_truth_table());

        // (q ∧ p) ∨ p ≡ p
        assert!(f1.is_equivalent(&f2), "{f1} != {f2}");
        assert!(f2.is_equivalent(&f1), "{f2} != {f1}");
    }

    #[test]
    fn rename_is_not_equivalent() {
        let p = Variable::with_data(1, 'p');
        let q = Variable::with_data(2, 'q');
        let r = Variable::with_data(3, 'r');

        let f1 = Formula::atom(p) & q;
        let f2 = Formula::atom(p) & r;

        assert!(!f1.is_equivalent(&f2), "{f1} == {f2}");
        assert!(!f2.is_equivalent(&f1), "{f2} == {f1}");
    }
}
