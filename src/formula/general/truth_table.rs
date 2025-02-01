//! Define [truth table](https://en.wikipedia.org/wiki/Truth_table)
//! for [`BoolFn`]-s.
use std::{fmt, hash::Hash};

use crate::{
    formula::Valuation,
    truth_table::{TruthTable, TruthTabled},
};

use super::formula::Formula;

impl<T> TruthTabled for Formula<T>
where
    T: Clone + Eq + Hash + fmt::Display,
{
    type TT = FormulaTruthTable<T>;

    fn get_truth_table(&self) -> Self::TT {
        use itertools::Itertools as _;

        let atoms = self.atoms();
        let arity = atoms.len();

        #[allow(clippy::manual_repeat_n)]
        let table: Vec<_> = std::iter::repeat([false, true])
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
            .collect();

        assert_eq!(table.len(), 1 << arity, "The table is complete");
        let atoms = atoms.iter().copied().cloned().collect();
        FormulaTruthTable { atoms, table }
    }
}

impl<T> TruthTable for FormulaTruthTable<T> {
    type Input = Vec<bool>;

    type Repr = Vec<Row>;

    fn iter(&self) -> impl Iterator<Item = &(Self::Input, bool)> {
        self.table.iter()
    }

    fn into_inner(self) -> Self::Repr {
        self.table
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
    table: Vec<Row>,
}

type Row = (Vec<bool>, bool);

impl<T> IntoIterator for FormulaTruthTable<T> {
    type Item = Row;

    type IntoIter = <Vec<Row> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.table.into_iter()
    }
}

impl<T> fmt::Display for FormulaTruthTable<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let atoms = &self.atoms;
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

        for atom in atoms {
            // FIXME: {atom:<padding_var$} is not working
            write!(f, "| {:<padding_var$} ", atom.to_string())?;
        }
        writeln!(f, "| {value_col} |")?;

        let arity = atoms.len();
        for _ in 0..=arity {
            write!(f, "|{:-<padding_sep$}", "")?;
        }
        write!(f, "|")?;

        let rows = &self.table;
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
