//! A [Formula](https://en.wikipedia.org/wiki/Propositional_formula) is a Boolean-valued
//! well-formed expression denoting a proposition and having as such
//! a [truth value](https://en.wikipedia.org/wiki/Truth_value).
use std::fmt;

use derive_where::derive_where;

use crate::connective::{
    functions, Associativity as _, Connective, Prioritized, Priority, TruthFn,
};

pub use super::{
    super::{atom::Atom, ops::*},
    connective::AnyConnective,
};

#[derive(Debug, Clone)]
#[derive_where(PartialEq; T: PartialEq + 'static)]
/// [`Formula`] is a well-formed expression constructed from
/// propositions or [variables][Atom]
/// and [logical operators][crate::connective::Connective].
///
/// Based on <https://docs.rs/logic/>
pub enum Formula<T> {
    /// Degenerate kind of [`Formula`] having fixed
    /// [truth value](https://en.wikipedia.org/wiki/Truth_value)
    /// without any [`Atom`]s.
    TruthValue(bool),

    /// The simplest type of [`Formula`] with no deeper propositional structure.
    /// <https://en.wikipedia.org/wiki/Atomic_formula>
    Atomic(T),

    /// <https://en.wikipedia.org/wiki/Negation>
    Not(Box<Self>),

    /// <https://en.wikipedia.org/wiki/Logical_conjunction>
    And(Box<Self>, Box<Self>),

    /// <https://en.wikipedia.org/wiki/Logical_disjunction>
    Or(Box<Self>, Box<Self>),

    /// <https://en.wikipedia.org/wiki/Exclusive_or>
    Xor(Box<Self>, Box<Self>),

    /// <https://en.wikipedia.org/wiki/Material_conditional>
    Implies(Box<Self>, Box<Self>),

    /// <https://en.wikipedia.org/wiki/Logical_biconditional>
    Equivalent(Box<Self>, Box<Self>),

    /// The operator is specified dynamically.
    Other(AnyConnective<Box<Self>, T>),
}

impl<T> From<bool> for Formula<T> {
    fn from(value: bool) -> Self {
        Self::TruthValue(value)
    }
}

impl<T: Atom> From<T> for Formula<T> {
    fn from(value: T) -> Self {
        Self::Atomic(value)
    }
}

impl<T: Atom> Formula<T> {
    /// Create an [atomic][Atom] formula.
    pub const fn atomic(atom: T) -> Self {
        Self::Atomic(atom)
    }
}

impl<T> Formula<T> {
    /// Get a top-level connective for a given [`Formula`] along with the operands.
    pub fn get_connective(&self) -> AnyConnective<&Self, T> {
        match self {
            Self::TruthValue(true) => AnyConnective::new_0(functions::Truth),
            Self::TruthValue(false) => AnyConnective::new_0(functions::Falsity),
            Self::Atomic(_) => AnyConnective::new_1(functions::LogicalIdentity, self),
            Self::Not(x) => AnyConnective::new_1(functions::Negation, x),
            Self::And(x1, x2) => AnyConnective::new_2(functions::Conjunction, (x1, x2)),
            Self::Or(x1, x2) => AnyConnective::new_2(functions::Disjunction, (x1, x2)),
            Self::Xor(x1, x2) => AnyConnective::new_2(functions::ExclusiveDisjunction, (x1, x2)),
            Self::Implies(x1, x2) => AnyConnective::new_2(functions::MaterialImplication, (x1, x2)),
            Self::Equivalent(x1, x2) => {
                AnyConnective::new_2(functions::LogicalBiconditional, (x1, x2))
            }
            Self::Other(inner) => inner.as_ref(),
        }
    }

    fn has_same_operation(&self, e: &Self) -> bool
    where
        T: 'static,
    {
        self.get_connective().clear_operands() == e.get_connective().clear_operands()
    }

    /// Whether the given [`Formula`] contains other [`Formula`]-s.
    pub const fn is_complex(&self) -> bool {
        !matches!(self, Self::TruthValue(..) | Self::Atomic(..))
    }

    fn priority(&self) -> Priority {
        self.get_connective().priority()
    }

    fn has_obvious_priority_over(&self, e: &Self) -> bool {
        self.priority() > e.priority()
    }
}

impl<T> From<Formula<T>> for AnyConnective<Formula<T>, T> {
    fn from(formula: Formula<T>) -> Self {
        match formula {
            Formula::TruthValue(true) => Self::new_0(functions::Truth),
            Formula::TruthValue(false) => Self::new_0(functions::Falsity),
            Formula::Atomic(_) => Self::new_1(functions::LogicalIdentity, formula),
            Formula::Not(x) => Self::new_1(functions::Negation, *x),
            Formula::And(x1, x2) => Self::new_2(functions::Conjunction, (*x1, *x2)),
            Formula::Or(x1, x2) => Self::new_2(functions::Disjunction, (*x1, *x2)),
            Formula::Xor(x1, x2) => Self::new_2(functions::ExclusiveDisjunction, (*x1, *x2)),
            Formula::Implies(x1, x2) => Self::new_2(functions::MaterialImplication, (*x1, *x2)),
            Formula::Equivalent(x1, x2) => Self::new_2(functions::LogicalBiconditional, (*x1, *x2)),
            Formula::Other(inner) => inner.map(|f| *f),
        }
    }
}

impl<T> Formula<T> {
    /// Create a [`Formula`] with the dynamic [`Connective`].
    pub fn with_connective<C>(connective: C, op1: Self, op2: Self) -> Self
    where
        C: Connective<2>
            + TruthFn<2, Self>
            + Prioritized
            + fmt::Debug
            + Clone
            + PartialEq
            + 'static,
    {
        Self::Other(AnyConnective::new_2(
            connective,
            (Box::new(op1), Box::new(op2)),
        ))
    }
}

impl<T> fmt::Display for Formula<T>
where
    T: fmt::Display + 'static,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Self::Atomic(p) = self {
            return write!(f, "{p}");
        }

        let conn = self.get_connective();

        match &conn {
            AnyConnective::Nullary(operator) => write!(f, "{}", operator.connective.notation()),
            AnyConnective::Unary(conn) => {
                let operator = &conn.connective;
                let [operand] = &conn.operands;

                let notation = operator.notation();
                if operand.has_obvious_priority_over(self) || operand.has_same_operation(self) {
                    write!(f, "{notation}{operand}")
                } else {
                    write!(f, "{notation}({operand})")
                }
            }
            AnyConnective::Binary(conn) => {
                let operator = &conn.connective;
                let [op1, op2] = &conn.operands;
                let is_associative = operator.is_associative();
                let notation = operator.notation();
                if op1.has_obvious_priority_over(self)
                    || (is_associative && op1.has_same_operation(self))
                {
                    write!(f, "{op1}{notation}")
                } else {
                    write!(f, "({op1}){notation}")
                }?;
                if op2.has_obvious_priority_over(self)
                    || (is_associative && op2.has_same_operation(self))
                {
                    write!(f, "{op2}")
                } else {
                    write!(f, "({op2})")
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Not;

    use super::{
        super::super::{atom::Atom, var::Variable},
        *,
    };

    #[test]
    fn operation() {
        let p = Variable::with_data(1, 'p');
        let q = Variable::with_data(2, 'q');
        let (p, q) = (Formula::atomic(p), Formula::atomic(q));
        assert_eq!(p.clone().not(), !p.clone());
        assert_eq!(p.clone().and(q.clone()), p.clone() & q.clone());
        assert_eq!(p.clone().or(q.clone()), p.clone() | q.clone());
        assert_eq!(p.clone().xor(q.clone()), p ^ q);
    }

    #[test]
    fn clone() {
        #[derive(Debug, Eq, PartialEq, Clone)]
        struct A(i32);

        impl Atom for A {}

        let e1 = Formula::atomic(A(1));
        let e2 = e1.clone();
        assert_eq!(e1, e2);
        drop(e2);
    }

    macro_rules! format_eq {
        ($e: expr, $s: expr) => {
            assert_eq!(format!("{}", $e), $s);
        };
    }

    #[test]
    fn formula_display() {
        let p = Variable::with_data(1, 'p');
        let q = Variable::with_data(2, 'q');
        let (p, q) = (Formula::atomic(p), Formula::atomic(q));

        format_eq!(Formula::TruthValue::<i32>(true), "⊤");
        format_eq!(Formula::TruthValue::<i32>(false), "⊥");
        format_eq!(p.clone().not(), "¬p");
        format_eq!(p.clone().and(q.clone()), "p∧q");
        format_eq!(p.clone().or(q.clone()), "p∨q");
        format_eq!(p.clone().xor(q.clone()), "p⊕q");
        format_eq!(p.clone().implies(q.clone()), "p→q");
        format_eq!(p.equivalent(q), "p↔q");
    }

    #[test]
    fn formula_priority_display() {
        let p = Variable::with_data(1, 'p');
        let q = Variable::with_data(2, 'q');
        let r = Variable::with_data(3, 'r');
        let (p, q, r) = (Formula::atomic(p), Formula::atomic(q), Formula::atomic(r));

        format_eq!(p.clone().not().not(), "¬¬p");

        let p_and_q = p.clone().and(q.clone());
        format_eq!(p_and_q.clone().not(), "¬(p∧q)");

        format_eq!(p_and_q.clone().and(r.clone()), "p∧q∧r");
        format_eq!(r.clone().and(p_and_q.clone()), "r∧p∧q");

        let p_or_q = p.clone().or(q.clone());
        format_eq!(p_or_q.clone().or(r.clone()), "p∨q∨r");
        format_eq!(r.clone().or(p_or_q), "r∨p∨q");

        format_eq!(p_and_q.clone().or(r.clone()), "(p∧q)∨r");
        format_eq!(r.clone().or(p_and_q), "r∨(p∧q)");

        let p_implies_q = p.clone().implies(q.clone());
        format_eq!(p_implies_q.clone().not(), "¬(p→q)");
        format_eq!(p_implies_q.clone().implies(r.clone()), "(p→q)→r");
        format_eq!(r.clone().implies(p_implies_q), "r→(p→q)");

        let p_equivalent_q = p.equivalent(q);
        format_eq!(p_equivalent_q.clone().not(), "¬(p↔q)");
        format_eq!(p_equivalent_q.clone().equivalent(r.clone()), "p↔q↔r");
        format_eq!(r.equivalent(p_equivalent_q), "r↔p↔q");
    }
}
