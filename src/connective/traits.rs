use crate::utils::{operation::Operation, upcast::UpcastFrom};

use super::{evaluation::Evaluable, notation::FunctionNotation};

#[auto_impl::auto_impl(&, Box)]
/// Enables the ability to combine propositional [`Evaluable`] into a single one.
///
/// A function that accepts `ARITY` [truth values](https://en.wikipedia.org/wiki/Truth_value) as input
/// and produces a unique [truth value](https://en.wikipedia.org/wiki/Truth_value) as output.
///
/// The [`ARITY`](https://en.wikipedia.org/wiki/Arity) is the number of arguments which this function accepts.
///
/// <https://en.wikipedia.org/wiki/Truth_function>
pub trait TruthFn<const ARITY: usize, E: Evaluable> {
    /// Try to reduce the propistional [`Evaluable`]-s
    /// into a single one by taking
    /// into account the partial knowledge of them.
    ///
    /// This is can also be viewed as
    /// [short-circuit evaluation](https://en.wikipedia.org/wiki/Short-circuit_evaluation).
    ///
    /// # Errors
    ///
    /// If the set of [`Evaluable`]-s is not reducible,
    /// produce the inputs wrapped in an `Err` as a last resort.
    fn fold(&self, terms: [E; ARITY]) -> Result<E, [E; ARITY]>;

    /// Compose an [`Evaluable`] from other [`Evaluable`]-s using self as a connective.
    fn compose(&self, terms: [E; ARITY]) -> E;
}

/// Extension of the [`TruthFn`] to create a [callable object][Operation]
/// to use with [`Evaluable`]-s.
///
/// Separated from the [parent trait][TruthFn] to allow for the latter
/// to be used with `auto_impl`.
pub trait TruthFnConnector<const ARITY: usize, E: Evaluable>: TruthFn<ARITY, E> {
    /// Returns a [callable object][Operation] which can
    /// be used to [compose][TruthFn::compose] a number of [`Evaluable`]-s.
    fn connector(self) -> Operation<ARITY, E>;
}

impl<const ARITY: usize, E: Evaluable, T> TruthFnConnector<ARITY, E> for T
where
    T: TruthFn<ARITY, E> + 'static,
{
    fn connector(self) -> Operation<ARITY, E> {
        Operation::new(Box::new(move |args| self.compose(args)))
    }
}

/// Special case of [`TruthFn`] dealing with plain `bool` values.
pub trait BoolFn<const ARITY: usize>: TruthFn<ARITY, bool> {}

impl<const ARITY: usize, T> BoolFn<ARITY> for T where T: TruthFn<ARITY, bool> {}

#[auto_impl::auto_impl(&, Box)]
/// A [logical constant](https://en.wikipedia.org/wiki/Logical_constant)
/// that can be used to connect logical formulae.
///
/// Its main usage is to convert to/from string representation of a formula.
pub trait Connective<const ARITY: usize>: BoolFn<ARITY> {
    /// The symbol (or name) most commonly used for the operation.
    fn notation(&self) -> FunctionNotation;

    /// The set of alternate symbols (names) which can be used
    /// instead of the [main one][Self::notation].
    ///
    /// Could be helpful in parsing the expressions.
    ///
    /// Also,
    /// [the polish notation]( https://en.wikipedia.org/wiki/J%C3%B3zef_Maria_Boche%C5%84ski#Pr%C3%A9cis_de_logique_math%C3%A9matique)
    /// included.
    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        None
    }
}

impl<'a, const N: usize, Atom: Connective<N> + 'a> UpcastFrom<Atom> for dyn Connective<N> + 'a {
    fn up_from(value: &Atom) -> &Self {
        value
    }

    fn up_from_mut(value: &mut Atom) -> &mut Self {
        value
    }
}

/// Helper trait to ease the instantiation of the `TruthFn`.
///
/// Every [`TruthFn`] is essentially a bunch of _static_ functions
/// without the need to create an _instance_. So, as a rule of thumb,
/// any instantiated type should have exactly one value (unit type, isomorphic to `()`).
///
/// But, in order to use the [`TruthFn`] in a dynamic context
/// we have to create some 'dummy' instance.
/// Usually, the [`Default`] trait can be used in such a case,
/// but using [`Default`] as a supertrait here disables the use of `Box<dyn TruthFn>`
/// because of the `Default: Sized` requirement.
///
/// Unfortunately it is not possible now in stable rust
/// to ensure the generic type is ZST, so whenever you use
/// the [`TruthFn`] and want to ensure it has zero size,
/// you have to:
/// 1. Require that the generic [`TruthFn`] is [`ZST`][crate::utils::Zst].
///    This is not used as a supertrait here deliberately to allow to use `dyn TruthFn`,
///    (as the `Zst` trait contains an associated constant preventing the dynamic object to create).
/// 2. Use the [`Zst::ASSERT_ZST`][crate::utils::Zst::ASSERT_ZST] in your function accepting
///    generic `TruthFn` to force the compiler to do `size_of` check.
pub trait InitFn {
    /// It is usually implemented as a shorthand for [`Default::default`].
    fn init() -> Self;
}

impl<T: Default> InitFn for T {
    fn init() -> Self {
        Self::default()
    }
}
