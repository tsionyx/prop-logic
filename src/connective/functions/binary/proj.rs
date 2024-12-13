//! Degenerate binary functions ignoring one of their arguments
//! to actually dependent on the one that is left.

use std::marker::PhantomData;

use super::super::{
    super::{Evaluable, FormulaComposer, Reducible},
    neg::Negation,
    BoolFn, Connective, Formula, FunctionNotation, TruthFn,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
/// [`Projection`] onto the _I_-th coordinate is a function that
/// takes an ordered pair like (x,y)
/// and just strips away everything but the _I_-th coordinate (0-based).
///
/// <https://en.wikipedia.org/wiki/Ordered_pair#Generalities>
pub struct Projection<const I: usize>;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
/// [`Projection`]-like operator that feed
/// the projection result further into some unary function.
///
/// The only non-trivial logical unary function is [`Negation`],
/// so it is used as the default transformation operator.
/// With this default setting, the operator's main purpose
/// is to act like __Fpq__ and __Gpq__ functions in terms of
/// [prefix logical notation](https://en.wikipedia.org/wiki/J%C3%B3zef_Maria_Boche%C5%84ski#Pr%C3%A9cis_de_logique_math%C3%A9matique).
pub struct ProjectAndUnary<const I: usize, UnaryOp: BoolFn<1> = Negation>(PhantomData<UnaryOp>);

impl<const I: usize, UnaryOp: BoolFn<1>> ProjectAndUnary<I, UnaryOp> {
    /// Create an instance of the [`ProjectAndUnary`].
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<const I: usize, UnaryOp: BoolFn<1>> Default for ProjectAndUnary<I, UnaryOp> {
    fn default() -> Self {
        Self::new()
    }
}

impl<const I: usize, UnaryOp: TruthFn<1>> BoolFn<2> for ProjectAndUnary<I, UnaryOp>
where
    Projection<I>: TruthFn<2>,
{
    fn eval(&self, values: [bool; 2]) -> bool {
        let project_result = Projection::<I>::init().eval(values);
        UnaryOp::init().eval([project_result])
    }
}

impl<const I: usize, E, UnaryOp> Reducible<2, E> for ProjectAndUnary<I, UnaryOp>
where
    E: Evaluable + Clone, // TODO: try to get rid of this `Clone` requirement
    UnaryOp: TruthFn<1> + Reducible<1, E>,
    Projection<I>: TruthFn<2> + Reducible<2, E>,
{
    fn try_reduce(&self, values: [E; 2]) -> Result<E, [E; 2]> {
        let expr = Projection::<I>::init().try_reduce(values.clone())?;
        UnaryOp::init().try_reduce([expr]).map_err(|_| values)
    }
}

impl<const I: usize, UnaryOp, T> FormulaComposer<2, T> for ProjectAndUnary<I, UnaryOp>
where
    UnaryOp: TruthFn<1> + FormulaComposer<1, T>,
    Projection<I>: TruthFn<2> + FormulaComposer<2, T>,
{
    fn compose(&self, expressions: [Formula<T>; 2]) -> Formula<T> {
        let expr = Projection::<I>::init().compose(expressions);
        UnaryOp::init().compose([expr])
    }
}

impl BoolFn<2> for Projection<0> {
    fn eval(&self, [val0, _]: [bool; 2]) -> bool {
        val0
    }
}

impl BoolFn<2> for Projection<1> {
    fn eval(&self, [_, val1]: [bool; 2]) -> bool {
        val1
    }
}

impl<E: Evaluable> Reducible<2, E> for Projection<0> {
    fn try_reduce(&self, [val0, _]: [E; 2]) -> Result<E, [E; 2]> {
        Ok(val0)
    }
}

impl<T> FormulaComposer<2, T> for Projection<0> {
    fn compose(&self, [expr0, _]: [Formula<T>; 2]) -> Formula<T> {
        expr0
    }
}

impl<E: Evaluable> Reducible<2, E> for Projection<1> {
    fn try_reduce(&self, [_, val1]: [E; 2]) -> Result<E, [E; 2]> {
        Ok(val1)
    }
}

impl<T> FormulaComposer<2, T> for Projection<1> {
    fn compose(&self, [_, expr1]: [Formula<T>; 2]) -> Formula<T> {
        expr1
    }
}

impl Connective<2> for Projection<0> {
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

impl Connective<2> for Projection<1> {
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

impl Connective<2> for ProjectAndUnary<0, Negation> {
    fn notation(&self) -> FunctionNotation {
        FunctionNotation::Polish('F')
    }
}

impl Connective<2> for ProjectAndUnary<1, Negation> {
    fn notation(&self) -> FunctionNotation {
        FunctionNotation::Polish('G')
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn projection_eval() {
        let x = Projection::<0>::init().bool_evaluator();
        assert!(!x([false, false]));
        assert!(!x([false, true]));
        assert!(x([true, false]));
        assert!(x([true, true]));

        let x = Projection::<1>::init().bool_evaluator();
        assert!(!x([false, false]));
        assert!(x([false, true]));
        assert!(!x([true, false]));
        assert!(x([true, true]));
    }

    #[test]
    fn projection_neg_eval() {
        let x = ProjectAndUnary::<0, Negation>::init().bool_evaluator();
        assert!(x([false, false]));
        assert!(x([false, true]));
        assert!(!x([true, false]));
        assert!(!x([true, true]));

        let x = ProjectAndUnary::<1, Negation>::init().bool_evaluator();
        assert!(x([false, false]));
        assert!(!x([false, true]));
        assert!(x([true, false]));
        assert!(!x([true, true]));
    }
}
