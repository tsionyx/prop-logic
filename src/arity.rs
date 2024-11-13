use crate::utils::dependent_array::{CheckedArray, CheckedStorage, Discriminant};

/// Implement size checking for the arrays of size `2^ARITY`.
///
/// Suitable to assert the number of combinations of inputs
/// of a [`TruthFn`][crate::TruthFn]-s for a given `ARITY`.
pub mod two_powers {
    use super::*;

    #[derive(Debug, Clone, Copy)]
    /// Discriminant for `2^ARITY` size.
    pub struct D;

    impl<const ARITY: usize> Discriminant<ARITY> for D {
        const ARR_SIZE: usize = (1 << ARITY);
    }

    impl CheckedArray<0> for D {
        type Array<T> = [T; 1];
    }

    impl CheckedArray<1> for D {
        type Array<T> = [T; 2];
    }

    impl CheckedArray<2> for D {
        type Array<T> = [T; 4];
    }

    impl CheckedArray<3> for D {
        type Array<T> = [T; 8];
    }

    const _ASSERT_0: () = <CheckedStorage<0, D, ()>>::ASSERT_SIZE;
    const _ASSERT_1: () = <CheckedStorage<1, D, ()>>::ASSERT_SIZE;
    const _ASSERT_2: () = <CheckedStorage<2, D, ()>>::ASSERT_SIZE;
    const _ASSERT_3: () = <CheckedStorage<3, D, ()>>::ASSERT_SIZE;
}

/// Implement size checking for the arrays of size `2^(2^ARITY)`.
///
/// Suitable to assert the number of possible
/// [`TruthFn`][crate::TruthFn]-s for a given `ARITY`.
pub mod two_powers_of_two_powers {
    use super::*;

    #[derive(Debug, Clone, Copy)]
    /// Discriminant for 2^(2^ARITY) size.
    pub struct D;

    impl<const ARITY: usize> Discriminant<ARITY> for D {
        const ARR_SIZE: usize = (1 << (1 << ARITY));
    }

    impl CheckedArray<0> for D {
        type Array<T> = [T; 2];
    }

    impl CheckedArray<1> for D {
        type Array<T> = [T; 4];
    }

    impl CheckedArray<2> for D {
        type Array<T> = [T; 16];
    }

    const _ASSERT_0: () = <CheckedStorage<0, D, ()>>::ASSERT_SIZE;
    const _ASSERT_1: () = <CheckedStorage<1, D, ()>>::ASSERT_SIZE;
    const _ASSERT_2: () = <CheckedStorage<2, D, ()>>::ASSERT_SIZE;
}

#[cfg(test)]
mod tests {
    use crate::CheckedStorage;

    use super::*;

    const _X0: CheckedStorage<0, two_powers::D, u8> = CheckedStorage::new([1]);
    const _X1: CheckedStorage<1, two_powers::D, u8> = CheckedStorage::new([1, 2]);
    const _X2: CheckedStorage<2, two_powers::D, u8> = CheckedStorage::new([1, 2, 3, 4]);

    const _Y0: CheckedStorage<0, two_powers_of_two_powers::D, u8> = CheckedStorage::new([1, 2]);
    const _Y1: CheckedStorage<1, two_powers_of_two_powers::D, u8> =
        CheckedStorage::new([1, 2, 3, 4]);
    const _Y2: CheckedStorage<2, two_powers_of_two_powers::D, u8> =
        CheckedStorage::new([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]);
}
