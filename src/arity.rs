use std::ops::Shl;

use typenum::{Const, Shleft, ToUInt, U, U1};

use crate::utils::dependent_array::SizeMapper;

#[derive(Debug, Copy, Clone)]
/// Implement size checking for the arrays of size `2^ARITY`.
///
/// Suitable to assert the number of combinations of inputs
/// of a [`TruthFn`][crate::connective::TruthFn]-s for a given `ARITY`.
pub struct TwoPower;

impl<const IN: usize> SizeMapper<IN> for TwoPower
where
    Const<IN>: ToUInt,
    U1: Shl<U<IN>>,
{
    // 1 << N
    type ArrSize = Shleft<U1, U<IN>>;
}

#[derive(Debug, Copy, Clone)]
/// Implement size checking for the arrays of size `2^(2^ARITY)`.
///
/// Suitable to assert the number of possible
/// [`TruthFn`][crate::connective::TruthFn]-s for a given `ARITY`.
pub struct TwoPowerOfTwoPower;

impl<const IN: usize> SizeMapper<IN> for TwoPowerOfTwoPower
where
    Const<IN>: ToUInt,
    U1: Shl<U<IN>>,
    U1: Shl<Shleft<U1, U<IN>>>,
{
    // 1 << (1 << N)
    type ArrSize = Shleft<U1, Shleft<U1, U<IN>>>;
}

#[cfg(test)]
mod tests {
    use crate::utils::dependent_array::CheckedStorage;

    use super::*;

    const _X0: CheckedStorage<u8, 0, TwoPower> = CheckedStorage([1].into());
    const _X1: CheckedStorage<u8, 1, TwoPower> = CheckedStorage([1, 2].into());
    const _X2: CheckedStorage<u8, 2, TwoPower> = CheckedStorage([1, 2, 3, 4].into());

    const _Y0: CheckedStorage<u8, 0, TwoPowerOfTwoPower> = CheckedStorage([1, 2].into());
    const _Y1: CheckedStorage<u8, 1, TwoPowerOfTwoPower> = CheckedStorage([1, 2, 3, 4].into());
    const _Y2: CheckedStorage<u8, 2, TwoPowerOfTwoPower> =
        CheckedStorage([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16].into());
}
