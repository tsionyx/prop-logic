pub mod dependent_array;
pub mod dyn_eq;
mod macros;
pub mod operation;
pub mod upcast;
pub mod zst;

pub(crate) use self::macros::cartesian_diag;
pub use self::zst::Zst;

pub use smol_str::SmolStr as Str;

/// Transpose a matrix
pub fn transpose<T>(matrix: Vec<Vec<T>>) -> Vec<Vec<T>> {
    if matrix.is_empty() {
        return vec![];
    }

    let no_columns = matrix[0].len();
    let mut transposed: Vec<_> = (0..no_columns)
        .map(|_| Vec::with_capacity(matrix.len()))
        .collect();
    for row in matrix {
        for (i, item) in row.into_iter().enumerate() {
            transposed[i].push(item);
        }
    }
    transposed
}
