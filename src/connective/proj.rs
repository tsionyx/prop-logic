use std::marker::PhantomData;

use super::{neg::Negation, Formula, TruthFunction};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
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
pub struct ProjectAndUnary<const I: usize, UnaryOp: TruthFunction<1> = Negation>(
    PhantomData<UnaryOp>,
);

impl<const I: usize, UnaryOp: TruthFunction<1>> ProjectAndUnary<I, UnaryOp> {
    /// Create an instance of the [`ProjectAndUnary`].
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<const I: usize, UnaryOp: TruthFunction<1>> Default for ProjectAndUnary<I, UnaryOp> {
    fn default() -> Self {
        Self::new()
    }
}

impl<const I: usize, UnaryOp: TruthFunction<1>> TruthFunction<2> for ProjectAndUnary<I, UnaryOp>
where
    Projection<I>: TruthFunction<2>,
{
    fn eval(values: [bool; 2]) -> bool {
        let project_result = Projection::<I>::eval(values);
        UnaryOp::eval([project_result])
    }

    fn apply<T>(expressions: [Formula<T>; 2]) -> Formula<T> {
        let expr = Projection::<I>::apply(expressions);
        UnaryOp::apply([expr])
    }
}

impl<const I: usize> TruthFunction<2> for Projection<I> {
    fn eval(values: [bool; 2]) -> bool {
        // TODO: check indices
        // ignores the first or second argument
        values[I]
    }

    fn apply<T>(expressions: [Formula<T>; 2]) -> Formula<T> {
        // TODO: check indices, do not Clone
        expressions[I].clone()
    }
}

// === The following implementations are degenerate ===
// impl Connective for Projection<0> {
//     const ARITY: usize = 2;
//
//     fn notation() -> FunctionNotation {
//         "π1".into()
//     }
//
//     fn alternate_notations() -> Option<Vec<FunctionNotation>> {
//         Some(vec!["πl".into(), "Ipq".into()])
//     }
// }
// impl Connective for Projection<1> {
//     const ARITY: usize = 2;
//
//     fn notation() -> FunctionNotation {
//         "π2".into()
//     }
//
//     fn alternate_notations() -> Option<Vec<FunctionNotation>> {
//         Some(vec!["πr".into(), "Hpq".into()])
//     }
// }
// impl Connective for ProjectAndUnary<0, Negation> {
//     const ARITY: usize = 2;
//
//     fn notation() -> FunctionNotation {
//         // only the polish notation available
//         // <https://en.wikipedia.org/wiki/J%C3%B3zef_Maria_Boche%C5%84ski#Pr%C3%A9cis_de_logique_math%C3%A9matique>
//         "Fpq".into()
//     }
// }
// impl Connective for ProjectAndUnary<1, Negation> {
//     const ARITY: usize = 2;
//
//     fn notation() -> FunctionNotation {
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
        let x = Projection::<0>::evaluator();
        assert!(!x([false, false]));
        assert!(!x([false, true]));
        assert!(x([true, false]));
        assert!(x([true, true]));

        let x = Projection::<1>::evaluator();
        assert!(!x([false, false]));
        assert!(x([false, true]));
        assert!(!x([true, false]));
        assert!(x([true, true]));
    }

    #[test]
    fn projection_neg_eval() {
        let x = ProjectAndUnary::<0, Negation>::evaluator();
        assert!(x([false, false]));
        assert!(x([false, true]));
        assert!(!x([true, false]));
        assert!(!x([true, true]));

        let x = ProjectAndUnary::<1, Negation>::evaluator();
        assert!(x([false, false]));
        assert!(!x([false, true]));
        assert!(x([true, false]));
        assert!(!x([true, true]));
    }
}
