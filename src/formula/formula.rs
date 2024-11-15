//! A [Formula](https://en.wikipedia.org/wiki/Propositional_formula) is a Boolean-valued
//! well-formed expression denoting a proposition and having as such
//! a [truth value](https://en.wikipedia.org/wiki/Truth_value).
use std::{fmt, sync::Arc};

use crate::connective::{functions, Associativity as _, Connective, Prioritized, Priority};

pub use super::{atom::Atom, connective::AnyConnective, ops::*};

#[derive(Debug, Eq, PartialEq)]
/// [`Formula`] is a well-formed expression constructed from
/// propositions or [variables][Atom]
/// and [logical operators][super::connective::Connective].
///
/// Based on <https://docs.rs/logic/>
pub enum Formula<T> {
    /// Degenerate kind of [`Formula`] having fixed
    /// [truth value](https://en.wikipedia.org/wiki/Truth_value)
    /// without any [`Atom`]s.
    TruthValue(bool),

    /// The simplest type of [`Formula`] with no deeper propositional structure.
    /// <https://en.wikipedia.org/wiki/Atomic_formula>
    Atomic(Arc<T>),

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
    Other(AnyConnective<Box<Self>>),
}

impl<T> From<bool> for Formula<T> {
    fn from(value: bool) -> Self {
        Self::TruthValue(value)
    }
}

impl<T: Atom> From<Arc<T>> for Formula<T> {
    fn from(value: Arc<T>) -> Self {
        Self::Atomic(value)
    }
}

impl<T: Atom> Formula<T> {
    /// Create an [atomic][Atom] formula.
    pub fn atomic(atom: T) -> Self {
        Self::Atomic(Arc::new(atom))
    }
}

impl<T> Formula<T> {
    /// Create a 'constant' [`Formula`] without any variables
    /// always holding a `true` value.
    pub const fn tautology() -> Self {
        Self::TruthValue(true)
    }

    /// Create a 'constant' [`Formula`] without any variables
    /// always holding a `false` value.
    pub const fn contradiction() -> Self {
        Self::TruthValue(false)
    }

    /// Get a top-level connective for a given [`Formula`].
    pub fn get_connective(&self) -> AnyConnective<&Self> {
        match self {
            Self::TruthValue(true) => AnyConnective::new_0::<functions::Truth>(),
            Self::TruthValue(false) => AnyConnective::new_0::<functions::Falsity>(),
            Self::Atomic(_) => AnyConnective::new_1::<functions::LogicalIdentity>(self),
            Self::Not(x) => AnyConnective::new_1::<functions::Negation>(x),
            Self::And(x1, x2) => AnyConnective::new_2::<functions::Conjunction>((x1, x2)),
            Self::Or(x1, x2) => AnyConnective::new_2::<functions::Disjunction>((x1, x2)),
            Self::Xor(x1, x2) => AnyConnective::new_2::<functions::ExclusiveDisjunction>((x1, x2)),
            Self::Implies(x1, x2) => {
                AnyConnective::new_2::<functions::MaterialImplication>((x1, x2))
            }
            Self::Equivalent(x1, x2) => {
                AnyConnective::new_2::<functions::LogicalBiconditional>((x1, x2))
            }
            Self::Other(inner) => inner.as_ref(),
        }
    }

    fn has_same_operation(&self, e: &Self) -> bool {
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

impl<T> Formula<T> {
    /// Create a [`Formula`] with the dynamic [`Connective`].
    pub fn with_connective<C>(op1: Self, op2: Self) -> Self
    where
        C: Connective<2> + Prioritized + fmt::Debug + Copy + 'static,
    {
        Self::Other(AnyConnective::new_2::<C>((Box::new(op1), Box::new(op2))))
    }
}

// implement Clone for Formula<T> no matter whether T is cloned
impl<T> Clone for Formula<T> {
    fn clone(&self) -> Self {
        match self {
            Self::TruthValue(t) => Self::TruthValue(*t),
            Self::Atomic(p) => Self::Atomic(p.clone()),
            Self::Not(e) => Self::Not(e.clone()),
            Self::And(e1, e2) => Self::And(e1.clone(), e2.clone()),
            Self::Or(e1, e2) => Self::Or(e1.clone(), e2.clone()),
            Self::Xor(e1, e2) => Self::Xor(e1.clone(), e2.clone()),
            Self::Implies(e1, e2) => Self::Implies(e1.clone(), e2.clone()),
            Self::Equivalent(e1, e2) => Self::Equivalent(e1.clone(), e2.clone()),
            Self::Other(op) => Self::Other(op.clone()),
        }
    }
}

impl<T> fmt::Display for Formula<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Self::Atomic(p) = self {
            return write!(f, "{}", p);
        }

        let conn = self.get_connective();

        match &conn {
            AnyConnective::Nullary(operator) => write!(f, "{}", operator.notation()),
            AnyConnective::Unary { operator, operand } => {
                if operand.has_obvious_priority_over(self) || operand.has_same_operation(self) {
                    write!(f, "{}{}", operator.notation(), operand)
                } else {
                    write!(f, "{}({})", operator.notation(), operand)
                }
            }
            AnyConnective::Binary {
                operator,
                operands: (op1, op2),
            } => {
                let is_associative = operator.is_associative();
                if op1.has_obvious_priority_over(self)
                    || (is_associative && op1.has_same_operation(self))
                {
                    write!(f, "{}{}", op1, operator.notation())
                } else {
                    write!(f, "({}){}", op1, operator.notation())
                }?;
                if op2.has_obvious_priority_over(self)
                    || (is_associative && op2.has_same_operation(self))
                {
                    write!(f, "{}", op2)
                } else {
                    write!(f, "({})", op2)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Not;

    use super::{
        super::{atom::Atom, var::Variable},
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
        #[derive(Debug, Eq, PartialEq)]
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
