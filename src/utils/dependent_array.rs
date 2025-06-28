pub use generic_array::{ArrayLength, GenericArray};

/// Defines the rule to map `IN` to the array size.
pub trait SizeMapper<const IN: usize> {
    /// Final expected array size.
    type ArrSize;
}

#[derive(Debug, Clone)]
/// Storage of the fixed number of `T` items that is dependent on parameter `IN`.
///
/// The `S` defines rules for how the `IN` parameter should map
/// on the actual size of the underlying [`GenericArray`].
///
/// The most straightforward use is to create a compile-time constant
/// to be able to enumerate all the items anywhere.
pub struct CheckedStorage<T, const N: usize, S>(pub GenericArray<T, S::ArrSize>)
where
    S: SizeMapper<N>,
    S::ArrSize: ArrayLength;

impl<T, const N: usize, S> AsRef<[T]> for CheckedStorage<T, N, S>
where
    S: SizeMapper<N>,
    S::ArrSize: ArrayLength,
{
    fn as_ref(&self) -> &[T] {
        self.0.as_slice()
    }
}
