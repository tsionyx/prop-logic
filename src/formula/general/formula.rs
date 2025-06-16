//! A [Formula](https://en.wikipedia.org/wiki/Propositional_formula) is a Boolean-valued
//! well-formed expression denoting a proposition and having as such
//! a [truth value](https://en.wikipedia.org/wiki/Truth_value).
use std::fmt::{self, Debug, Display};

use derive_where::derive_where;

use crate::connective::{
    functions, Associativity as _, Connective, Prioritized, Priority, TruthFn,
};

pub use super::{
    super::{atom::Atom, ops::*},
    connective::{AnyConnective, DynConnective},
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
    Dynamic(AnyConnective<Box<Self>, T>),
}

impl<T> From<bool> for Formula<T> {
    fn from(value: bool) -> Self {
        Self::truth(value)
    }
}

impl<T: Atom> From<T> for Formula<T> {
    fn from(value: T) -> Self {
        Self::atom(value)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
/// The wrapper around the atomic value of a [`Formula`]
/// to represent the simplest structures:
/// - an [atomic value][Formula::Atomic] ('p');
/// - a [negated][Formula::Not] [atomic value][Formula::Atomic] ('¬p').
///
/// See the [`Formula::as_directed_atom`] for usage.
///
/// It is somewhat similar to the [`Literal`][super::super::Literal]
/// but the wrapped value is not constrained to the [`Variable`][super::super::Variable].
pub enum Directed<T> {
    /// The atomic value itself.
    Straight(T),
    /// The negated atomic value.
    Negated(T),
}

impl<T> AsRef<T> for Directed<T> {
    fn as_ref(&self) -> &T {
        match self {
            Self::Straight(p) | Self::Negated(p) => p,
        }
    }
}

impl<T> Formula<T> {
    /// Create a constant _truth_ or _falsity_ formula.
    pub const fn truth(value: bool) -> Self {
        Self::TruthValue(value)
    }

    /// Create an [atomic][Atom] formula.
    pub const fn atom(atom: T) -> Self {
        Self::Atomic(atom)
    }

    #[expect(clippy::should_implement_trait)]
    /// Create a [negated formula][Self::Not].
    pub fn not(op: Self) -> Self {
        Self::Not(Box::new(op))
    }

    /// Create a [conjunction][Self::And] of two formulae.
    pub fn and(op1: Self, op2: Self) -> Self {
        Self::And(Box::new(op1), Box::new(op2))
    }

    /// Create a [disjunction][Self::Or] of two formulae.
    pub fn or(op1: Self, op2: Self) -> Self {
        Self::Or(Box::new(op1), Box::new(op2))
    }

    /// Create an [exclusive disjunction][Self::Xor] of two formulae.
    pub fn xor(op1: Self, op2: Self) -> Self {
        Self::Xor(Box::new(op1), Box::new(op2))
    }

    /// Create a formula of [implication][Self::Implies]
    /// from the first formula (antecedent) to the second one (consequent).
    ///
    /// Be aware that this operation is **not commutative**
    /// (i.e. `a -> b != b -> a` in common case).
    pub fn implies(op1: Self, op2: Self) -> Self {
        Self::Implies(Box::new(op1), Box::new(op2))
    }

    /// Create a formula of [equivalence][Self::Equivalent] between the two formulae.
    pub fn equivalent(op1: Self, op2: Self) -> Self {
        Self::Equivalent(Box::new(op1), Box::new(op2))
    }
}

impl<T> Formula<T> {
    /// Transform the inner parts of a [`Formula`]
    /// preserving its structure.
    pub fn map<F>(self, mut transform: F) -> Self
    where
        F: FnMut(Self) -> Self,
    {
        match self {
            Self::TruthValue(_) | Self::Atomic(_) => transform(self),
            Self::Not(f) => Self::not(transform(*f)),
            Self::And(f1, f2) => Self::and(transform(*f1), transform(*f2)),
            Self::Or(f1, f2) => Self::or(transform(*f1), transform(*f2)),
            Self::Xor(f1, f2) => Self::xor(transform(*f1), transform(*f2)),
            Self::Implies(f1, f2) => Self::implies(transform(*f1), transform(*f2)),
            Self::Equivalent(f1, f2) => Self::equivalent(transform(*f1), transform(*f2)),
            Self::Dynamic(conn) => Self::Dynamic(conn.map(|f| Box::new(transform(*f)))),
        }
    }

    /// Transform a [`Formula`] by reversing order of its operands
    /// if it is a binary function. Otherwise, just return it as is.
    pub fn swap_operands(self) -> Self {
        match self {
            Self::TruthValue(_) | Self::Atomic(_) => self,
            Self::Not(f) => Self::Not(f),
            Self::And(f1, f2) => Self::And(f2, f1),
            Self::Or(f1, f2) => Self::Or(f2, f1),
            Self::Xor(f1, f2) => Self::Xor(f2, f1),
            Self::Implies(f1, f2) => Self::Implies(f2, f1),
            Self::Equivalent(f1, f2) => Self::Equivalent(f2, f1),
            Self::Dynamic(inner) => Self::Dynamic(inner.swap_operands()),
        }
    }

    /// Get a top-level connective for a given [`Formula`] along with the operands.
    pub fn get_connective(&self) -> AnyConnective<&Self, T> {
        match self {
            Self::TruthValue(true) => AnyConnective::nullary(functions::Truth),
            Self::TruthValue(false) => AnyConnective::nullary(functions::Falsity),
            Self::Atomic(_) => AnyConnective::unary(functions::LogicalIdentity, self),
            Self::Not(x) => AnyConnective::unary(functions::Negation, x),
            Self::And(x1, x2) => AnyConnective::binary(functions::Conjunction, (x1, x2)),
            Self::Or(x1, x2) => AnyConnective::binary(functions::Disjunction, (x1, x2)),
            Self::Xor(x1, x2) => AnyConnective::binary(functions::ExclusiveDisjunction, (x1, x2)),
            Self::Implies(x1, x2) => {
                AnyConnective::binary(functions::MaterialImplication, (x1, x2))
            }
            Self::Equivalent(x1, x2) => {
                AnyConnective::binary(functions::LogicalBiconditional, (x1, x2))
            }
            Self::Dynamic(inner) => inner.get_borrowed(),
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

    /// Whether the given [`Formula`] is [dynamic][Self::Dynamic].
    pub const fn is_dynamic(&self) -> bool {
        matches!(self, Self::Dynamic(_))
    }

    fn priority(&self) -> Priority {
        self.get_connective().priority()
    }

    fn has_obvious_priority_over(&self, e: &Self) -> bool {
        self.priority() > e.priority()
    }

    /// Represent a [`Formula`] as a [`Directed`] atomic value if possible.
    pub fn as_directed_atom(&self) -> Option<Directed<&T>> {
        if let Self::Atomic(p) = self {
            Some(Directed::Straight(p))
        } else if let Self::Not(f) = self {
            if let Self::Atomic(p) = f.as_ref() {
                Some(Directed::Negated(p))
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl<T> From<Formula<T>> for AnyConnective<Box<Formula<T>>, T> {
    fn from(formula: Formula<T>) -> Self {
        match formula {
            Formula::TruthValue(true) => Self::nullary(functions::Truth),
            Formula::TruthValue(false) => Self::nullary(functions::Falsity),
            Formula::Atomic(_) => Self::unary(functions::LogicalIdentity, Box::new(formula)),
            Formula::Not(x) => Self::unary(functions::Negation, x),
            Formula::And(x1, x2) => Self::binary(functions::Conjunction, (x1, x2)),
            Formula::Or(x1, x2) => Self::binary(functions::Disjunction, (x1, x2)),
            Formula::Xor(x1, x2) => Self::binary(functions::ExclusiveDisjunction, (x1, x2)),
            Formula::Implies(x1, x2) => Self::binary(functions::MaterialImplication, (x1, x2)),
            Formula::Equivalent(x1, x2) => Self::binary(functions::LogicalBiconditional, (x1, x2)),
            Formula::Dynamic(inner) => inner,
        }
    }
}

impl<T> From<Formula<T>> for AnyConnective<Formula<T>, T> {
    fn from(formula: Formula<T>) -> Self {
        AnyConnective::<Box<Formula<T>>, T>::from(formula).map(|f| *f)
    }
}

impl<T> Formula<T> {
    /// Create a new [`Formula`] using the _nullary_ [`Connective`] (a constant).
    pub fn nullary<C>(connective: C) -> Self
    where
        C: Connective<0> + TruthFn<0, Self> + Prioritized + Debug + Clone + PartialEq + 'static,
    {
        Self::Dynamic(AnyConnective::nullary(connective))
    }

    /// Create a new [`Formula`] using the _unary_ [`Connective`]
    /// to transform a given formula.
    pub fn unary<C>(connective: C, f: Self) -> Self
    where
        C: Connective<1> + TruthFn<1, Self> + Prioritized + Debug + Clone + PartialEq + 'static,
    {
        Self::Dynamic(AnyConnective::unary(connective, Box::new(f)))
    }

    /// Create a new [`Formula`] using the _binary_ [`Connective`]
    /// to combine two given formulae.
    pub fn binary<C>(connective: C, f1: Self, f2: Self) -> Self
    where
        C: Connective<2> + TruthFn<2, Self> + Prioritized + Debug + Clone + PartialEq + 'static,
    {
        Self::Dynamic(AnyConnective::binary(
            connective,
            (Box::new(f1), Box::new(f2)),
        ))
    }
}

impl<T> Display for Formula<T>
where
    T: Display + 'static,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Self::Atomic(p) = self {
            return Display::fmt(p, f);
        }

        let print_operand = |sub: &Self, is_associative_op, f: &mut fmt::Formatter<'_>| {
            let skip_parentheses = sub.has_obvious_priority_over(self)
                || (!f.alternate() && is_associative_op && sub.has_same_operation(self));

            if skip_parentheses {
                Display::fmt(&sub, f)
            } else {
                write!(f, "(")?;
                Display::fmt(&sub, f)?;
                write!(f, ")")
            }
        };

        match self.get_connective() {
            AnyConnective::Nullary(operator) => Display::fmt(&operator.connective.notation(), f),
            AnyConnective::Unary(DynConnective {
                connective,
                operands: [operand],
            }) => {
                Display::fmt(&connective.notation(), f)?;
                print_operand(operand, true, f)
            }
            AnyConnective::Binary(DynConnective {
                connective,
                operands: [op1, op2],
            }) => {
                let is_associative = connective.is_associative();
                print_operand(op1, is_associative, f)?;
                Display::fmt(&connective.notation(), f)?;
                print_operand(op2, is_associative, f)
            }
        }
    }
}

impl<T: PartialEq> Formula<T> {
    /// Get all the atomic values of the [`Formula`].
    pub fn atoms(&self) -> Vec<&T> {
        match self {
            Self::TruthValue(_) | Self::Dynamic(AnyConnective::Nullary(_)) => vec![],
            Self::Atomic(atom) => vec![atom],

            Self::Not(f)
            | Self::Dynamic(AnyConnective::Unary(DynConnective { operands: [f], .. })) => f.atoms(),

            Self::And(f1, f2)
            | Self::Or(f1, f2)
            | Self::Xor(f1, f2)
            | Self::Implies(f1, f2)
            | Self::Equivalent(f1, f2)
            | Self::Dynamic(AnyConnective::Binary(DynConnective {
                operands: [f1, f2], ..
            })) => {
                let mut atoms = f1.atoms();
                for atom in f2.atoms() {
                    if !atoms.contains(&atom) {
                        atoms.push(atom);
                    }
                }
                atoms
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Not;

    use super::{super::super::var::Variable, *};

    #[test]
    fn operation() {
        let p = Variable::with_data(1, 'p');
        let q = Variable::with_data(2, 'q');
        let (p, q) = (Formula::atom(p), Formula::atom(q));
        assert_eq!(p.clone().not(), !p.clone());
        assert_eq!(p.clone().and(q.clone()), p.clone() & q.clone());
        assert_eq!(p.clone().or(q.clone()), p.clone() | q.clone());
        assert_eq!(p.clone().xor(q.clone()), p ^ q);
    }

    #[test]
    fn clone() {
        #[derive(Debug, Eq, PartialEq, Clone)]
        struct A(i32);

        let e1 = Formula::atom(A(1));
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
        let (p, q) = (Formula::atom(p), Formula::atom(q));

        format_eq!(Formula::<i32>::truth(true), "⊤");
        format_eq!(Formula::<i32>::truth(false), "⊥");
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
        let (p, q, r) = (Formula::atom(p), Formula::atom(q), Formula::atom(r));

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
