//! Define [truth table](https://en.wikipedia.org/wiki/Truth_table)
//! for [`Formula`]-s.
use std::{fmt, hash::Hash, sync::Arc};

use crate::{
    formula::Valuation,
    truth_table::{TruthTable, TruthTabled},
    utils::transpose,
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

        if arity == 0 {
            let valuation = Valuation::<T>::empty();
            if let Self::TruthValue(value) = self.interpret(&valuation) {
                return FormulaTruthTable {
                    columns: vec![],
                    values: vec![value],
                };
            }
            panic!("Cannot evaluate formala with no atoms");
        }

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
        let (rows, values) = table.into_iter().unzip();
        let columns = transpose(rows);
        let atoms: Vec<_> = atoms.iter().map(|&a| Arc::new(a.clone())).collect();
        let columns = columns
            .into_iter()
            .zip(&atoms)
            .map(|(column, atom)| (Arc::clone(atom), column))
            .collect();
        FormulaTruthTable { columns, values }
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
        if self.columns.is_empty() {
            Rows::Constant(self.values.first().copied())
        } else {
            let rows = self.values.iter().enumerate().map(move |(i, &result)| {
                let values_row = self.columns.iter().map(move |(atom, assignments)| {
                    let assignment = assignments[i];
                    (atom.as_ref(), assignment)
                });
                #[expect(trivial_casts)]
                (Box::new(values_row) as Self::Row<'_>, result)
            });
            Rows::WithAtoms(rows)
        }
    }
}

pub enum Rows<'a, I, T>
where
    I: Iterator<Item = (LazyRow<'a, T>, bool)>,
    T: 'a,
{
    Constant(Option<bool>),
    WithAtoms(I),
}

impl<'a, I, T> Iterator for Rows<'a, I, T>
where
    I: Iterator<Item = (LazyRow<'a, T>, bool)>,
    T: 'a,
{
    type Item = (LazyRow<'a, T>, bool);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Rows::Constant(value) => value.take().map(|val| {
                #[expect(trivial_casts)]
                let it = Box::new(std::iter::empty()) as Box<dyn Iterator<Item = (&T, bool)> + 'a>;
                (it, val)
            }),
            Rows::WithAtoms(rows) => rows.next(),
        }
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
    columns: Vec<(Arc<T>, Vec<bool>)>,
    values: Vec<bool>,
}

impl<T: PartialEq> FormulaTruthTable<T> {
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
        self == other
        // TODO:
        // - remove the non-significant atoms (by shrinking the table twice for each of it)
        // - compare the tables have the same columns with respect to their order:
        // let all_rows: Vec<_> = self
        //     .iter()
        //     .map(|(row, value)| {
        //         let row: Vec<_> = row.map(|(_, assignment)| assignment).collect();
        //         (row, value)
        //     })
        //     .collect();
    }
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
    fn commutative_equivalence() {
        let p = Variable::with_data(1, 'p');
        let q = Variable::with_data(2, 'q');

        let f1 = Formula::atom(p) & q;
        let f2 = Formula::atom(q) & p;

        assert!(f1.is_equivalent(&f2), "{f1} != {f2}");
        assert!(f2.is_equivalent(&f1), "{f2} != {f1}");
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
