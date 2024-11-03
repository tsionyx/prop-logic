use std::{borrow::Borrow, collections::HashMap as Map, hash::Hash, ops::Not, sync::Arc};

use super::{
    atom::AtomValue,
    formula::Formula,
    ops::{And, Equivalent, Implies, Or, Xor},
};

#[derive(Debug, Default)]
/// Mapping the [`Atoms`]s of a [`Formula`]
/// to one of the [truth values](https://en.wikipedia.org/wiki/Truth_value).
///
/// <https://en.wikipedia.org/wiki/Valuation_(logic)>
pub struct Valuation<T> {
    values: Map<Arc<T>, AtomValue>,
}

impl<T> Valuation<T>
where
    T: Eq + Hash,
{
    /// Construct a new `Valuation`.
    pub fn new() -> Self {
        Self { values: Map::new() }
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

            Self::Other { .. } => {
                todo!()
            }
        }
    }
}
