#![allow(
    dead_code,
    missing_docs,
    missing_copy_implementations,
    missing_debug_implementations,
    non_snake_case
)]

use std::ops::Shl;

use generic_array::{arr, ArrayLength, GenericArray};
use typenum::{Const, Shleft, ToUInt, Unsigned, U, U1};

#[derive(Debug, Clone)]
pub struct CheckedStorage<T, const N: usize, S>(pub GenericArray<T, S::Out>)
where
    S: SizeMapper<N>,
    S::Out: ArrayLength;

/// Implement size checking for the arrays of size `2^(2^ARITY)`.
///
/// Suitable to assert the number of possible
/// [`TruthFn`][crate::connective::TruthFn]-s for a given `ARITY`.
pub struct TwoPowerOfTwoPower;

impl<const IN: usize> SizeMapper<IN> for TwoPowerOfTwoPower
where
    Const<IN>: ToUInt,
    U1: Shl<U<IN>>,
    U1: Shl<Shleft<U1, U<IN>>, Output: Unsigned>,
{
    // 1 << (1 << N)
    type Out = Shleft<U1, Shleft<U1, U<IN>>>;
}

/// Defines the rule to map the `IN` constant to the `Out` typenum.
pub trait SizeMapper<const IN: usize> {
    type Out;
}

pub struct PowerOfTwo;

impl<const IN: usize> SizeMapper<IN> for PowerOfTwo
where
    Const<IN>: ToUInt,
    U1: Shl<U<IN>>,
    // UNCOMMENT NEXT LINE TO GET `overflow evaluating the requirement ...`
    // Shleft<U1, U<IN>>: Unsigned,
{
    // 1 << N
    type Out = Shleft<U1, U<IN>>;
}

fn create_generic_array<const N: usize>()
where
    // creating a GenericArray of length
    // <PowerOfTwo as SizeMapper<N>>::Out
    // requires it to be `Unsigned`
    PowerOfTwo: SizeMapper<N, Out: Unsigned>,
{
    println!(
        "The length for N={N}: {}",
        <PowerOfTwo as SizeMapper<N>>::Out::to_usize()
    );
}

fn main() {
    const _X0: CheckedStorage<u8, 0, PowerOfTwo> = CheckedStorage(arr![1]);
    const _X1: CheckedStorage<u8, 1, PowerOfTwo> = CheckedStorage(arr![1, 2]);
    let _X2: CheckedStorage<u8, 2, PowerOfTwo> = CheckedStorage([1, 2, 3, 4].into());

    let _Y0: CheckedStorage<u8, 0, TwoPowerOfTwoPower> = CheckedStorage([1, 2].into());
    let _Y1: CheckedStorage<u8, 1, TwoPowerOfTwoPower> = CheckedStorage([1, 2, 3, 4].into());
    let _Y2: CheckedStorage<u8, 2, TwoPowerOfTwoPower> =
        CheckedStorage([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16].into());

    create_generic_array::<0>();
    create_generic_array::<1>();
    create_generic_array::<2>();
    create_generic_array::<3>();
    create_generic_array::<16>();
    create_generic_array::<63>();
    create_generic_array::<64>();
    create_generic_array::<100>();
}

struct Struct<N>
where
    N: Unsigned,
    typenum::U2: typenum::Pow<N, Output: ArrayLength>,
{
    data: GenericArray<f64, typenum::Exp<typenum::U2, N>>,
}
