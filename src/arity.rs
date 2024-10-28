use crate::utils::dependent_array::{CheckedArray, SizedArray, VerifySize};

/// Implement size checking for 2^(2^ARITY) arrays.
///
/// Suitable to assert the number of possible
/// [`TruthFunction`][crate::TruthFunction]-s for a given `ARITY`.
pub mod two_powers_of_two_powers {
    use super::*;

    #[derive(Debug, Clone, Copy)]
    /// Marker type to implement size checking for nullary functions.
    pub struct Arity0;

    impl CheckedArray for Arity0 {
        type Array<T> = [T; 2];
    }

    #[derive(Debug, Clone, Copy)]
    /// Marker type to implement size checking for unary functions.
    pub struct Arity1;

    impl CheckedArray for Arity1 {
        type Array<T> = [T; 4];
    }

    #[derive(Debug, Clone, Copy)]
    /// Marker type to implement size checking for binary functions.
    pub struct Arity2;

    impl CheckedArray for Arity2 {
        type Array<T> = [T; 16];
    }

    trait DoubleTwoPowerArray<const IN: usize>: CheckedArray {
        const DOUBLE_POWER: usize = (1 << (1 << IN));
    }

    impl<const IN: usize, ARR> DoubleTwoPowerArray<IN> for ARR where ARR: CheckedArray {}

    impl<const ARITY: usize, N: DoubleTwoPowerArray<ARITY>> VerifySize<ARITY> for N {
        const ASSERT_SIZE: () = assert!(<N::Array<()> as SizedArray>::SIZE == N::DOUBLE_POWER);
    }
}
