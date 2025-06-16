//! Degenerate binary functions ignoring one of their arguments
//! to actually dependent on the one that is left.

use std::marker::PhantomData;

use super::super::{
    super::{Connective, Evaluable, FunctionNotation, InitFn, TruthFn},
    neg::Negation,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// [`Projection`] onto the _I_-th coordinate is a function that
/// takes an ordered pair like (x,y)
/// and just strips away everything but the _I_-th coordinate (0-based).
///
/// <https://en.wikipedia.org/wiki/Ordered_pair#Generalities>
pub struct Projection<const I: usize>;

/// The projection function returning always its first argument
/// and ignoring the others.
///
/// The function is associative and can be used not only as the binary
/// but as the function with any `ARITY >= 2` when applied sequentially.
pub type First = Projection<0>;

/// The projection function returning always its last argument
/// and ignoring the others.
///
/// The function is associative and can be used not only as the binary
/// but as the function with any `ARITY >= 2`  when applied sequentially
/// (therefore its name is the `Last`, not the `Second`).
pub type Last = Projection<1>;

impl<E: Evaluable> TruthFn<2, E> for First {
    fn fold(&self, [val0, _]: [E; 2]) -> Result<E, [E; 2]> {
        Ok(val0)
    }

    fn compose(&self, [val0, _]: [E; 2]) -> E {
        val0
    }
}

impl<E: Evaluable> TruthFn<2, E> for Last {
    fn fold(&self, [_, val1]: [E; 2]) -> Result<E, [E; 2]> {
        Ok(val1)
    }

    fn compose(&self, [_, val1]: [E; 2]) -> E {
        val1
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
/// [`Projection`]-like operator that feed
/// the projection result further into some unary function.
///
/// The only non-trivial logical unary function is [`Negation`],
/// so it is used as the default transformation operator.
/// With this default setting, the operator's main purpose
/// is to act like __Fpq__ and __Gpq__ functions in terms of
/// [prefix logical notation](https://en.wikipedia.org/wiki/J%C3%B3zef_Maria_Boche%C5%84ski#Pr%C3%A9cis_de_logique_math%C3%A9matique).
pub struct ProjectAndUnary<const I: usize, UnaryOp = Negation>(PhantomData<UnaryOp>);

/// The projection function returning always the [`Negation`]
/// of its _first_ argument and ignoring the others.
pub type NotFirst = ProjectAndUnary<0, Negation>;

/// The projection function returning always the [`Negation`]
/// of its _second_ argument and ignoring the others.
///
/// The function is **not** associative and, when applied sequentially,
/// can produce the negation of the specified argument
/// using the proper order of operations
/// (therefore its name is the `NotSecond`, not the `NotLast`,
/// as could be mistakenly deduced from its binary negation [`Last`]).
pub type NotSecond = ProjectAndUnary<1, Negation>;

impl<const I: usize, UnaryOp> ProjectAndUnary<I, UnaryOp> {
    /// Create an instance of the [`ProjectAndUnary`].
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<const I: usize, UnaryOp> Default for ProjectAndUnary<I, UnaryOp> {
    fn default() -> Self {
        Self::new()
    }
}

impl<const I: usize, E, UnaryOp> TruthFn<2, E> for ProjectAndUnary<I, UnaryOp>
where
    E: Evaluable + Clone, // TODO: try to get rid of this `Clone` requirement
    UnaryOp: TruthFn<1, E> + InitFn,
    Projection<I>: TruthFn<2, E>,
{
    fn fold(&self, values: [E; 2]) -> Result<E, [E; 2]> {
        let expr = Projection::<I> {}.fold(values.clone())?;
        UnaryOp::init().fold([expr]).map_err(|_| values)
    }

    fn compose(&self, terms: [E; 2]) -> E {
        let expr = Projection::<I> {}.compose(terms);
        UnaryOp::init().compose([expr])
    }

    fn fold_or_compose(&self, terms: [E; 2]) -> E {
        let expr = Projection::<I> {}.fold_or_compose(terms);
        UnaryOp::init().fold_or_compose([expr])
    }
}

impl Connective<2> for First {
    fn notation(&self) -> FunctionNotation {
        FunctionNotation::symbolic_str("π1")
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            FunctionNotation::symbolic_str("πl"),
            FunctionNotation::common("left projection"),
            FunctionNotation::Polish('I'),
        ])
    }
}

impl Connective<2> for Last {
    fn notation(&self) -> FunctionNotation {
        FunctionNotation::symbolic_str("π2")
    }

    fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
        Some(vec![
            FunctionNotation::symbolic_str("πr"),
            FunctionNotation::common("right projection"),
            FunctionNotation::Polish('H'),
        ])
    }
}

impl Connective<2> for NotFirst {
    fn notation(&self) -> FunctionNotation {
        FunctionNotation::Polish('F')
    }
}

impl Connective<2> for NotSecond {
    fn notation(&self) -> FunctionNotation {
        FunctionNotation::Polish('G')
    }
}

#[cfg(test)]
mod tests {
    use super::{super::super::super::TruthFnConnector as _, *};

    #[test]
    fn projection_eval() {
        let x = First {}.connector();
        assert!(!x([false, false]));
        assert!(!x([false, true]));
        assert!(x([true, false]));
        assert!(x([true, true]));

        let x = Last {}.connector();
        assert!(!x([false, false]));
        assert!(x([false, true]));
        assert!(!x([true, false]));
        assert!(x([true, true]));
    }

    #[test]
    fn projection_neg_eval() {
        let x = NotFirst::new().connector();
        assert!(x([false, false]));
        assert!(x([false, true]));
        assert!(!x([true, false]));
        assert!(!x([true, true]));

        let x = NotSecond::new().connector();
        assert!(x([false, false]));
        assert!(!x([false, true]));
        assert!(x([true, false]));
        assert!(!x([true, true]));
    }
}
