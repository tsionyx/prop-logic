//! A [truth table](https://en.wikipedia.org/wiki/Truth_table)
//! is a mathematical table which sets out the functional values
//! of logical expressions on each combination of their functional arguments.
//!
//! A truth table has one column for each input variable (for example, A and B),
//! and one final column showing all of the possible results of the logical operation
//! that the table represents (for example, A XOR B).
//! Each row of the truth table contains one possible configuration of the input variables
//! (for instance, A=true, B=false), and the result of the operation for those values.

/// Defines the operation of getting the [truth table](https://en.wikipedia.org/wiki/Truth_table).
///
/// Also, the `TruthTabled::is_equivalent` method has a default implementation
/// of comparing the truth tables of two functions.
pub trait TruthTabled<const ARITY: usize = 0> {
    /// The [`TruthTable`] type associated with the function.
    type TT: TruthTable<ARITY>;

    /// Generate a [truth table](https://en.wikipedia.org/wiki/Truth_table)
    /// for as the **key** (boolean arguments)-**value** (function result)
    /// ordered map.
    fn get_truth_table(&self) -> Self::TT;

    /// Check if the truth tables of two functions are equivalent.
    fn is_equivalent<Rhs>(&self, other: &Rhs) -> bool
    where
        Rhs: TruthTabled<ARITY, TT = Self::TT>,
    {
        self.get_truth_table().values() == other.get_truth_table().values()
    }
}

/// Generic `TruthTable` to allow to implement the trait
/// for both [`BoolFn`][crate::connective::BoolFn]-s and arbitrary [`Formula`][crate::Formula]-s.
///
/// TODO: the constant should be of type `Option<usize>`
/// to allow indetermine ARITY
pub trait TruthTable<const ARITY: usize = 0> {
    /// The input values holding a set of `bool` values.
    type Row<'a>
    where
        Self: 'a;

    /// Iterate over the rows of the truth table.
    fn iter(&self) -> impl Iterator<Item = (Self::Row<'_>, bool)>;

    /// Iterate over the `bool` values of the truth table
    /// in the default order (the sequence of incrementing binary numbers).
    fn iter_values(&self) -> impl Iterator<Item = bool> {
        self.iter().map(|(_k, v)| v)
    }

    /// Convert the whole table into the ordered sequence
    /// of bool results in the default order (the sequence of incrementing binary numbers).
    fn values(&self) -> Vec<bool>
    where
        Self: Sized,
    {
        self.iter_values().collect()
    }
}
