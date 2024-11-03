//! A [Formula](https://en.wikipedia.org/wiki/Propositional_formula) is a Boolean-valued
//! well-formed expression denoting a proposition and having as such
//! a [truth value](https://en.wikipedia.org/wiki/Truth_value).
use std::{fmt, mem, sync::Arc};

pub use super::{super::connective::Connective, atom::Atom, connective::DynOperator, ops::*};

#[derive(Debug, Eq, PartialEq)]
/// [`Formula`] is a well-formed expression constructed from
/// propositions or [variables][Atom]
/// and [logical operators][super::connective::Connective].
///
/// Based on <https://docs.rs/logic/>
pub enum Formula<T> {
    /// Degenerate kind of [`Formula`] having fixed
    /// [truth value](<https://en.wikipedia.org/wiki/Truth_value)
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
    Other {
        /// The [`Connective`] for the operands.
        operator: DynOperator,
        /// Sub-formulas.
        operands: (Box<Self>, Box<Self>),
    },
}

impl<T: Atom> Formula<T> {
    /// Create an [atomic][Atom] formula.
    pub fn atomic(p: Arc<T>) -> Self {
        Self::Atomic(p)
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

    fn has_same_operation(&self, e: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(e)
    }

    /// Whether the given [`Formula`] contains other [`Formula`]-s.
    pub const fn is_complex(&self) -> bool {
        !matches!(self, Self::TruthValue(..) | Self::Atomic(..))
    }

    const fn priority(&self) -> u8 {
        match *self {
            Self::TruthValue(..) | Self::Atomic(..) => 0,
            Self::Not(..) => 1,
            Self::And(..) | Self::Or(..) | Self::Xor(..) => 2,
            Self::Implies(..) | Self::Equivalent(..) => 3,
            Self::Other { .. } => {
                todo!()
            }
        }
    }

    const fn has_obvious_priority_over(&self, e: &Self) -> bool {
        self.priority() < e.priority()
    }
}

impl<T> Formula<T> {
    /// Create a [`Formula`] with the dynamic [`Connective`].
    pub fn with_connective<C>(op1: Self, op2: Self) -> Self
    where
        C: Connective<2> + fmt::Debug + Copy + 'static,
    {
        Self::Other {
            operator: DynOperator::new::<C>(),
            operands: (Box::new(op1), Box::new(op2)),
        }
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
            Self::Other {
                operator,
                operands: (e1, e2),
            } => Self::Other {
                operator: operator.clone(),
                operands: (e1.clone(), e2.clone()),
            },
        }
    }
}

impl<T> fmt::Display for Formula<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TruthValue(t) => write!(f, "{}", if *t { 'T' } else { 'F' }),
            Self::Atomic(p) => write!(f, "{}", p),
            Self::Not(e) => {
                if e.has_obvious_priority_over(self) || e.has_same_operation(self) {
                    write!(f, "¬{}", e)
                } else {
                    write!(f, "¬({})", e)
                }
            }
            Self::And(e1, e2) => {
                if e1.has_obvious_priority_over(self) || e1.has_same_operation(self) {
                    write!(f, "{}∧", e1)
                } else {
                    write!(f, "({})∧", e1)
                }?;
                if e2.has_obvious_priority_over(self) || e2.has_same_operation(self) {
                    write!(f, "{}", e2)
                } else {
                    write!(f, "({})", e2)
                }
            }
            Self::Or(e1, e2) => {
                if e1.has_obvious_priority_over(self) || e1.has_same_operation(self) {
                    write!(f, "{}∨", e1)
                } else {
                    write!(f, "({})∨", e1)
                }?;
                if e2.has_obvious_priority_over(self) || e2.has_same_operation(self) {
                    write!(f, "{}", e2)
                } else {
                    write!(f, "({})", e2)
                }
            }
            Self::Xor(e1, e2) => {
                if e1.has_obvious_priority_over(self) || e1.has_same_operation(self) {
                    write!(f, "{}⊕", e1)
                } else {
                    write!(f, "({})⊕", e1)
                }?;
                if e2.has_obvious_priority_over(self) || e2.has_same_operation(self) {
                    write!(f, "{}", e2)
                } else {
                    write!(f, "({})", e2)
                }
            }
            Self::Implies(e1, e2) => {
                if e1.has_obvious_priority_over(self) {
                    write!(f, "{}⇒", e1)
                } else {
                    write!(f, "({})⇒", e1)
                }?;
                if e2.has_obvious_priority_over(self) || e2.has_same_operation(self) {
                    write!(f, "{}", e2)
                } else {
                    write!(f, "({})", e2)
                }
            }
            Self::Equivalent(e1, e2) => {
                if e1.has_obvious_priority_over(self) {
                    write!(f, "{}⇔", e1)
                } else {
                    write!(f, "({})⇔", e1)
                }?;
                if e2.has_obvious_priority_over(self) || e2.has_same_operation(self) {
                    write!(f, "{}", e2)
                } else {
                    write!(f, "({})", e2)
                }
            }

            Self::Other { .. } => {
                todo!()
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
        let p = Arc::new(Variable::with_data(1, 'p'));
        let q = Arc::new(Variable::with_data(2, 'q'));
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

        let e1 = Formula::atomic(Arc::new(A(1)));
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
        let p = Arc::new(Variable::with_data(1, 'p'));
        let q = Arc::new(Variable::with_data(2, 'q'));
        let (p, q) = (Formula::atomic(p), Formula::atomic(q));

        format_eq!(Formula::TruthValue::<i32>(true), "T");
        format_eq!(Formula::TruthValue::<i32>(false), "F");
        format_eq!(p.clone().not(), "¬p");
        format_eq!(p.clone().and(q.clone()), "p∧q");
        format_eq!(p.clone().or(q.clone()), "p∨q");
        format_eq!(p.clone().xor(q.clone()), "p⊕q");
        format_eq!(p.clone().implies(q.clone()), "p⇒q");
        format_eq!(p.equivalent(q), "p⇔q");
    }

    #[test]
    fn formula_priority_display() {
        let p = Arc::new(Variable::with_data(1, 'p'));
        let q = Arc::new(Variable::with_data(2, 'q'));
        let r = Arc::new(Variable::with_data(3, 'r'));
        let (p, q, r) = (Formula::atomic(p), Formula::atomic(q), Formula::Atomic(r));

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
        format_eq!(p_implies_q.clone().not(), "¬(p⇒q)");
        format_eq!(p_implies_q.clone().implies(r.clone()), "(p⇒q)⇒r");
        format_eq!(r.clone().implies(p_implies_q), "r⇒p⇒q");

        let p_equivalent_q = p.equivalent(q);
        format_eq!(p_equivalent_q.clone().not(), "¬(p⇔q)");
        format_eq!(p_equivalent_q.clone().equivalent(r.clone()), "(p⇔q)⇔r");
        format_eq!(r.equivalent(p_equivalent_q), "r⇔p⇔q");
    }
}
