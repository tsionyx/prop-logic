//! Degenerate binary functions ignoring one of their arguments
//! to actually dependent on the one that is left.

use std::marker::PhantomData;

use super::{super::Evaluation, neg::Negation, BoolFn, Formula, TruthFn};

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

impl<const I: usize, UnaryOp: TruthFn<1>> TruthFn<2> for ProjectAndUnary<I, UnaryOp>
where
    Projection<I>: TruthFn<2>,
{
    fn init() -> Self {
        Self::new()
    }

    fn reduce<T>(&self, values: [Evaluation<T>; 2]) -> Option<Evaluation<T>>
    where
        Self: Sized,
        T: std::ops::Not<Output = T>,
    {
        let expr = Projection::<I>::init().reduce(values)?;
        UnaryOp::init().reduce([expr])
    }

    fn apply<T>(&self, expressions: [Formula<T>; 2]) -> Formula<T> {
        let expr = Projection::<I>::init().apply(expressions);
        UnaryOp::init().apply([expr])
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

impl TruthFn<2> for Projection<0> {
    fn init() -> Self {
        Self
    }

    fn reduce<T>(&self, [val0, _]: [Evaluation<T>; 2]) -> Option<Evaluation<T>>
    where
        Self: Sized,
        T: std::ops::Not<Output = T>,
    {
        Some(val0)
    }

    fn apply<T>(&self, [expr0, _]: [Formula<T>; 2]) -> Formula<T> {
        expr0
    }
}

impl TruthFn<2> for Projection<1> {
    fn init() -> Self {
        Self
    }

    fn reduce<T>(&self, [_, val1]: [Evaluation<T>; 2]) -> Option<Evaluation<T>>
    where
        Self: Sized,
        T: std::ops::Not<Output = T>,
    {
        Some(val1)
    }

    fn apply<T>(&self, [_, expr1]: [Formula<T>; 2]) -> Formula<T> {
        expr1
    }
}

// === The following implementations are degenerate ===
// impl Connective<2> for Projection<0> {
//     fn notation(&self) -> FunctionNotation {
//         "π1".into()
//     }
//
//     fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
//         Some(vec!["πl".into(), "Ipq".into()])
//     }
// }
// impl Connective<2> for Projection<1> {
//     fn notation(&self) -> FunctionNotation {
//         "π2".into()
//     }
//
//     fn alternate_notations(&self) -> Option<Vec<FunctionNotation>> {
//         Some(vec!["πr".into(), "Hpq".into()])
//     }
// }
// impl Connective<2> for ProjectAndUnary<0, Negation> {
//     fn notation(&self) -> FunctionNotation {
//         // only the polish notation available
//         // <https://en.wikipedia.org/wiki/J%C3%B3zef_Maria_Boche%C5%84ski#Pr%C3%A9cis_de_logique_math%C3%A9matique>
//         "Fpq".into()
//     }
// }
// impl Connective<2> for ProjectAndUnary<1, Negation> {
//     fn notation(&self) -> FunctionNotation {
//         // only the polish notation available
//         // <https://en.wikipedia.org/wiki/J%C3%B3zef_Maria_Boche%C5%84ski#Pr%C3%A9cis_de_logique_math%C3%A9matique>
//         "Gpq".into()
//     }
// }

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
