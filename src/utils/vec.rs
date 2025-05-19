/// Compare two `Vec<T: PartialEq>` for set equality (ignoring order)
/// by checking that every element in one vector appears
/// the same number of times in the other vector.
///
/// This approach has `O(n²)` complexity because it requires each element
/// to be compared once with elements in the other vector,
/// rather than counting occurrences multiple times.
///
/// It uses a boolean vector to track which elements have been "consumed" from the second vector.
///
/// The solution only require `T: PartialEq` and don't need
/// `Hash` or `Ord` trait bounds, making it more generic,
/// but it only suitable for small vectors.
pub fn vec_eq_unordered<T: PartialEq>(a: &[T], b: &[T]) -> bool {
    if a.len() != b.len() {
        return false;
    }

    let mut b_used: Vec<bool> = vec![false; b.len()];

    'iter_a: for item_a in a {
        for (item_b, used) in b.iter().zip(&mut b_used) {
            if !*used && item_a == item_b {
                *used = true;
                continue 'iter_a;
            }
        }
        return false;
    }

    debug_assert!(
        b_used.iter().all(|&used| used),
        "Not all elements in b were used"
    );
    true
}

#[derive(Debug, Clone, Eq)]
/// The `Vec` without any particular order.
/// This is useful for comparing two vectors for set equality.
pub struct UnsortedVec<T>(pub Vec<T>);

impl<T: PartialEq> PartialEq for UnsortedVec<T> {
    fn eq(&self, other: &Self) -> bool {
        vec_eq_unordered(&self.0, &other.0)
    }
}

impl<T> From<Vec<T>> for UnsortedVec<T> {
    fn from(value: Vec<T>) -> Self {
        Self(value)
    }
}

impl<T> From<UnsortedVec<T>> for Vec<T> {
    fn from(value: UnsortedVec<T>) -> Self {
        value.0
    }
}

impl<T> AsRef<[T]> for UnsortedVec<T> {
    fn as_ref(&self) -> &[T] {
        self.0.as_ref()
    }
}

impl<T> FromIterator<T> for UnsortedVec<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<T> IntoIterator for UnsortedVec<T> {
    type Item = T;

    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[expect(dead_code)]
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eq_unordered() {
        let vec1 = vec![1, 2, 3, 2];
        let vec2 = vec![2, 3, 1, 2];
        let vec3 = vec![1, 2, 3];

        assert!(
            vec_eq_unordered(&vec1, &vec2),
            "same elements and same counts should be equal"
        );
        assert!(
            !vec_eq_unordered(&vec1, &vec3),
            "different counts should not be equal"
        );

        let vec4 = vec!["hello", "world"];
        let vec5 = vec!["world", "hello"];
        assert!(vec_eq_unordered(&vec4, &vec5)); // works with any PartialEq type
    }
}
