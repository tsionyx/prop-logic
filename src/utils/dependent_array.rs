use std::{marker::PhantomData, ops::Deref};

/// A trait containing a single associated [array type][SizedArray]
/// which is guaranteed to have the constant size.
pub trait CheckedArray {
    /// The [`SizedArray`]
    type Array<T>: SizedArray + TryFrom<Vec<T>> + Into<Vec<T>> + IntoIterator<Item = T>;
}

/// Only implemented for arrays to allow to use their sizes
/// as a constant at compile time.
pub trait SizedArray {
    /// The size of the array.
    const SIZE: usize;
}

// Implement for all fixed-size arrays
impl<const N: usize, T> SizedArray for [T; N] {
    const SIZE: usize = N;
}

/// Helper trait to define the rules to assert
/// the subtupes of [`CheckedArray`] has the size dependent on the `IN` constant.
pub trait VerifySize<const IN: usize> {
    /// The assert expression.
    ///
    /// It should be called in the client code explicitly
    /// for the specific type implemented on
    /// to enable the compile-time assertion.
    const ASSERT_SIZE: ();
}

#[derive(Debug, Copy, Clone)]
/// Storage of the fixed number of `T` items that is dependent on parameter `IN`.
///
/// The `ARR` defines rules for how the `IN` parameter should map
/// on the actual size of the underlying [`CheckedArray`].
///
/// The most straightforward use is to create a compile-time constant
/// to be able to enumerate all the items anywhere.
pub struct CheckedStorage<const IN: usize, ARR, T>
where
    ARR: CheckedArray,
{
    items: ARR::Array<T>,
    _dummy: PhantomData<ARR>,
}

impl<const IN: usize, ARR, T> CheckedStorage<IN, ARR, T>
where
    ARR: CheckedArray,
{
    /// Create a new instance of [`CheckedStorage`] from the given array.
    pub const fn new(items: ARR::Array<T>) -> Self {
        Self {
            items,
            _dummy: PhantomData,
        }
    }

    /// Return inner [array][CheckedArray::Array].
    pub fn into_inner(self) -> ARR::Array<T> {
        self.items
    }
}

impl<const IN: usize, ARR, T> Deref for CheckedStorage<IN, ARR, T>
where
    ARR: CheckedArray,
{
    type Target = ARR::Array<T>;

    fn deref(&self) -> &Self::Target {
        &self.items
    }
}

impl<const IN: usize, ARR, T> CheckedStorage<IN, ARR, T>
where
    ARR: CheckedArray + VerifySize<IN>,
{
    /// Call this constant to assert that the table contains
    /// exactly the expected number of storage defined in [`VerifySize`]
    /// for your [`CheckedArray`].
    pub const ASSERT_SIZE: () = ARR::ASSERT_SIZE;
}
