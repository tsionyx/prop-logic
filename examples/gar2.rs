#![allow(dead_code, missing_docs)]

// See more on the problem of failed compilation:
// - https://github.com/rust-lang/rust/issues/60471
// - https://github.com/paholg/typenum/issues/75

use generic_array::{typenum::*, ArrayLength, GenericArray};

#[derive(Debug)]
struct Test<N>(GenericArray<f64, Exp<U2, N>>)
where
    U2: Pow<N, Output: ArrayLength>;

fn from_slice<N>(data: &[f64]) -> Test<N>
where
    U2: Pow<N, Output: ArrayLength>,
{
    Test::<N>(GenericArray::<f64, <U2 as Pow<N>>::Output>::from_slice(data).clone())
}

impl<N> Test<N>
where
    N: Unsigned + IsLessOrEqual<U2, Output = True>,
    U2: Pow<N, Output: ArrayLength>,
{
    fn from_s(data: &[f64]) -> Test<N> {
        // UNCOMMENT NEXT LINE TO GET `overflow evaluating the requirement ...`
        // return from_slice(data);
        Test::<N>(GenericArray::<f64, <U2 as Pow<N>>::Output>::from_slice(data).clone())
    }
}

fn main() {
    let data = &[0., 1., 2., 3.];
    let test = from_slice::<U2>(data);
    dbg!(test);
}
