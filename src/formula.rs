//! A [Formula](https://en.wikipedia.org/wiki/Propositional_formula) is a Boolean-valued
//! well-formed expression denoting a proposition and having as such
//! a [truth value](https://en.wikipedia.org/wiki/Truth_value).
use std::{
    borrow::Borrow,
    collections::HashMap,
    fmt,
    hash::Hash,
    mem,
    ops::{BitAnd, BitOr, BitXor, Not},
    sync::Arc,
};

pub use super::ops::*;

#[derive(Debug, Eq, PartialEq, Hash)]
/// [`Formula`] is a well-formed expression constructed from
/// propositions or [variables][Atom]
/// and [logical operators][super::connective::Connective].
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
}

/// An atomic entity with no deeper propositional structure.
///
/// For propositional logic, a [propositional variable][Variable]
/// is often more briefly referred to as an [atomic formula][Atom],
/// but, more precisely, a [propositional variable][Variable]
/// is not an atomic formula but a formal expression that denotes an atomic formula.
pub trait Atom {}

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

    const fn priority(&self) -> u8 {
        match *self {
            Self::TruthValue(..) | Self::Atomic(..) => 0,
            Self::Not(..) => 1,
            Self::And(..) => 2,
            Self::Or(..) => 3,
            Self::Xor(..) => 4,
            Self::Implies(..) => 5,
            Self::Equivalent(..) => 6,
        }
    }

    const fn vague_priority(&self) -> u8 {
        match *self {
            Self::TruthValue(..) | Self::Atomic(..) => 0,
            Self::Not(..) => 1,
            Self::And(..) | Self::Or(..) | Self::Xor(..) => 2,
            Self::Implies(..) | Self::Equivalent(..) => 3,
        }
    }

    const fn has_obvious_priority_over(&self, e: &Self) -> bool {
        self.vague_priority() < e.vague_priority()
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
                if e.has_obvious_priority_over(self) || e.priority() == self.priority() {
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
        }
    }
}

impl<T> Not for Formula<T> {
    type Output = Self;
    fn not(self) -> Self::Output {
        Self::Not(Box::new(self))
    }
}

impl<RHS, T> BitAnd<RHS> for Formula<T>
where
    RHS: Into<Self>,
{
    type Output = Self;
    fn bitand(self, e: RHS) -> Self::Output {
        self.and(e.into())
    }
}

impl<RHS, T> BitOr<RHS> for Formula<T>
where
    RHS: Into<Self>,
{
    type Output = Self;
    fn bitor(self, e: RHS) -> Self::Output {
        self.or(e.into())
    }
}

impl<RHS, T> BitXor<RHS> for Formula<T>
where
    RHS: Into<Self>,
{
    type Output = Self;
    fn bitxor(self, e: RHS) -> Self::Output {
        self.xor(e.into())
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
/// A propositional variable is a
/// formal expression that denotes an [atomic formula][Atom].
///
/// It is a basic building block
/// of the propositional calculus.  
///
/// <https://en.wikipedia.org/wiki/Propositional_variable>
pub struct Variable<T> {
    id: u64,
    extra: Option<T>,
}

#[derive(Debug, Copy, Clone)]
/// Uninhabited type.
pub enum Void {}

/// The most primitive instantiation of the [`Variable`]
/// without any additional info attached
/// optimized for memory usage.
pub type Var = Variable<Void>;

impl<T> Variable<T> {
    /// Create a new [`Variable`] without any extra data associated with it.
    pub const fn new(id: u64) -> Self {
        Self { id, extra: None }
    }

    /// Create a new [`Variable`]
    /// associating some extra data with it.
    pub const fn with_data(id: u64, data: T) -> Self {
        Self {
            id,
            extra: Some(data),
        }
    }
}

impl<T> Atom for Variable<T> {}

impl<T> fmt::Display for Variable<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(extra) = &self.extra {
            if f.alternate() {
                write!(f, "{} (id={})", extra, self.id)
            } else {
                write!(f, "{}", extra)
            }
        } else {
            write!(f, "{}", self.id)
        }
    }
}

#[derive(Debug, Default, Copy, Clone)]
/// The concrete [truth value](https://en.wikipedia.org/wiki/Truth_value)
/// associated with the [`Atom`].
///
/// It can be no value assigned.
pub struct AtomValue {
    value: Option<bool>,
}

impl AtomValue {
    /// Get the [truth value](https://en.wikipedia.org/wiki/Truth_value)
    /// if there is any.
    pub const fn get(&self) -> Option<bool> {
        self.value
    }
}

#[derive(Debug, Default)]
/// Mapping the [`Atoms`]s of a [`Formula`]
/// to one of the [truth values](https://en.wikipedia.org/wiki/Truth_value).
///
/// <https://en.wikipedia.org/wiki/Valuation_(logic)>
pub struct Valuation<T> {
    values: HashMap<Arc<T>, AtomValue>,
}

impl<T> Valuation<T>
where
    T: Eq + Hash,
{
    /// Construct a new `Valuation`.
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    /// Retrieve a truth value of a specific [`Atom`] if any.
    pub fn get_assignment<Q>(&self, key: &Q) -> Option<bool>
    where
        Arc<T>: Borrow<Q>,
        Q: Eq + Hash,
    {
        self.values.get(key).and_then(AtomValue::get)
    }

    /// Set a truth value to a specific [`Atom`].
    pub fn assign(&mut self, key: Arc<T>, value: bool) {
        let _previous_value = self.values.insert(key, AtomValue { value: Some(value) });
    }
}

#[derive(Debug)]
enum EvaluationResult<T> {
    Partial(Formula<T>),
    Terminal(bool),
}

impl<T> EvaluationResult<T> {
    const fn tautology() -> Self {
        Self::Terminal(true)
    }

    const fn contradiction() -> Self {
        Self::Terminal(false)
    }
}

impl<T> From<Formula<T>> for EvaluationResult<T> {
    fn from(value: Formula<T>) -> Self {
        Self::Partial(value)
    }
}

impl<T> Formula<T> {
    #[must_use]
    /// Trying to get the value of [`Formula`]
    /// by reducing it using available [Atom]'s [`Valuation`].
    ///
    /// If the [`Valuation`] is incomplete,
    /// the reduced [`Formula`] is going to be produced
    /// by doing short-circuit calculation wherever possible.
    pub fn interpret(&self, i12n: &Valuation<T>) -> Self
    where
        T: Eq + Hash,
    {
        match self._interpret(i12n) {
            EvaluationResult::Partial(formula) => formula,
            EvaluationResult::Terminal(val) => Self::TruthValue(val),
        }
    }

    fn _interpret(&self, i12n: &Valuation<T>) -> EvaluationResult<T>
    where
        T: Eq + Hash,
    {
        use EvaluationResult as E;

        match self {
            Self::TruthValue(val) => E::Terminal(*val),
            Self::Atomic(p) => i12n
                .get_assignment(p.as_ref())
                .map_or_else(|| self.clone().into(), E::Terminal),
            Self::Not(e) => match e._interpret(i12n) {
                E::Partial(e) => e.not().into(),
                E::Terminal(val) => E::Terminal(!val),
            },
            Self::And(e1, e2) => match (e1._interpret(i12n), e2._interpret(i12n)) {
                (E::Partial(e1), E::Partial(e2)) => e1.and(e2).into(),
                (E::Partial(expr), E::Terminal(leaf))
                | // **conjunction** is _commutative_
                (E::Terminal(leaf), E::Partial(expr)) => {
                    if leaf {
                        expr.into()
                    } else {
                        E::contradiction()
                    }
                }
                (E::Terminal(e1_val), E::Terminal(e2_val)) => {
                    E::Terminal(e1_val & e2_val)
                }
            },
            Self::Or(e1, e2) => match (e1._interpret(i12n), e2._interpret(i12n)) {
                (E::Partial(e1), E::Partial(e2)) => e1.or(e2).into(),
                (E::Partial(expr), E::Terminal(leaf))
                | // **disjunction** is _commutative_
                (E::Terminal(leaf), E::Partial(expr)) => {
                    if leaf {
                        E::tautology()
                    } else {
                        expr.into()
                    }
                }
                (E::Terminal(e1_val), E::Terminal(e2_val)) => {
                    E::Terminal(e1_val | e2_val)
                }
            },
            Self::Xor(e1, e2) => match (e1._interpret(i12n), e2._interpret(i12n)) {
                (E::Partial(e1), E::Partial(e2)) => e1.xor(e2).into(),
                (E::Partial(expr), E::Terminal(leaf))
                | // **exclusive disjunction** is _commutative_
                (E::Terminal(leaf), E::Partial(expr)) => {
                    if leaf { expr.not() } else { expr }.into()
                }
                (E::Terminal(e1_val), E::Terminal(e2_val)) => {
                    E::Terminal(e1_val ^ e2_val)
                }
            },
            Self::Implies(e1, e2) => match (e1._interpret(i12n), e2._interpret(i12n)) {
                (E::Partial(e1), E::Partial(e2)) => e1.implies(e2).into(),
                (E::Partial(e1), E::Terminal(e2_val)) => {
                    if e2_val {
                        E::tautology()
                    } else {
                        e1.not().into()
                    }
                }
                (E::Terminal(e1_val), E::Partial(e2)) => {
                    if e1_val {
                        e2.into()
                    } else {
                        // <https://en.wikipedia.org/wiki/Vacuous_truth>
                        E::tautology()
                    }
                }
                (E::Terminal(e1_val), E::Terminal(e2_val)) => E::Terminal(!e1_val | e2_val),
            },
            Self::Equivalent(e1, e2) => {
                match (e1._interpret(i12n), e2._interpret(i12n)) {
                    (E::Partial(e1), E::Partial(e2)) => {
                        e1.equivalent(e2).into()
                    }
                    (E::Partial(expr), E::Terminal(leaf))
                    | // **equivalence** is _commutative_
                    (E::Terminal(leaf), E::Partial(expr)) => {
                        if leaf { expr } else { expr.not() }.into()
                    }
                    (E::Terminal(e1_val), E::Terminal(e2_val)) => {
                        E::Terminal(e1_val == e2_val)
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn size_var_contains_no_label() {
        let var = Var::new(12);
        assert_eq!(mem::size_of_val(&var), 8);
    }
}
