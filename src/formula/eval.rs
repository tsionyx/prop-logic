use std::{borrow::Borrow, collections::HashMap as Map, hash::Hash, sync::Arc};

use super::{
    atom::AtomValue,
    formula::Formula,
    ops::{And, Equivalent, Implies, Not, Or, Xor},
};

#[derive(Debug)]
/// Mapping the [`Atoms`]s of a [`Formula`]
/// to one of the [truth values](https://en.wikipedia.org/wiki/Truth_value).
///
/// <https://en.wikipedia.org/wiki/Valuation_(logic)>
pub struct Valuation<T> {
    values: Map<Arc<T>, AtomValue>,
}

impl<T> Default for Valuation<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Valuation<T> {
    /// Construct a new `Valuation`.
    pub fn new() -> Self {
        Self { values: Map::new() }
    }
}

impl<T> Valuation<T>
where
    T: Eq + Hash,
{
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
        match self.try_interpret(i12n) {
            EvaluationResult::Partial(formula) => formula,
            EvaluationResult::Terminal(val) => Self::TruthValue(val),
        }
    }

    fn try_interpret(&self, i12n: &Valuation<T>) -> EvaluationResult<T>
    where
        T: Eq + Hash,
    {
        use EvaluationResult as E;

        match self {
            Self::TruthValue(val) => E::Terminal(*val),
            Self::Atomic(p) => i12n
                .get_assignment(p.as_ref())
                .map_or_else(|| self.clone().into(), E::Terminal),
            Self::Not(e) => match e.try_interpret(i12n) {
                E::Partial(e) => e.not().into(),
                E::Terminal(val) => E::Terminal(!val),
            },
            Self::And(e1, e2) => {
                match (e1.try_interpret(i12n), e2.try_interpret(i12n)) {
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
            }
            }
            Self::Or(e1, e2) => {
                match (e1.try_interpret(i12n), e2.try_interpret(i12n)) {
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
            }
            }
            Self::Xor(e1, e2) => {
                match (e1.try_interpret(i12n), e2.try_interpret(i12n)) {
                (E::Partial(e1), E::Partial(e2)) => e1.xor(e2).into(),
                (E::Partial(expr), E::Terminal(leaf))
                | // **exclusive disjunction** is _commutative_
                (E::Terminal(leaf), E::Partial(expr)) => {
                    if leaf { expr.not() } else { expr }.into()
                }
                (E::Terminal(e1_val), E::Terminal(e2_val)) => {
                    E::Terminal(e1_val ^ e2_val)
                }
            }
            }
            Self::Implies(e1, e2) => {
                match (e1.try_interpret(i12n), e2.try_interpret(i12n)) {
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
                }
            }
            Self::Equivalent(e1, e2) => {
                match (e1.try_interpret(i12n), e2.try_interpret(i12n)) {
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

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicU64, Ordering};

    use super::{super::Variable, *};

    static VAR_ID_COUNTER: AtomicU64 = AtomicU64::new(4);

    fn get_var(name: char) -> Variable<char> {
        let id = match name {
            'a' => 0,
            'b' => 1,
            'c' => 2,
            'd' => 3,
            _ => VAR_ID_COUNTER.fetch_add(1, Ordering::Relaxed),
        };

        Variable::with_data(id, name)
    }

    fn partial_valuation() -> Valuation<Variable<char>> {
        let mut val = Valuation::new();
        val.assign(Arc::new(get_var('a')), true);
        val.assign(Arc::new(get_var('b')), false);
        val.assign(Arc::new(get_var('c')), false);
        val.assign(Arc::new(get_var('d')), true);
        val
    }

    #[test]
    fn tautology() {
        let f = Formula::tautology();
        assert_eq!(f.interpret(&partial_valuation()), Formula::TruthValue(true));
    }

    #[test]
    fn contradiction() {
        let f = Formula::contradiction();
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::TruthValue(false)
        );
    }

    #[test]
    fn known_atom() {
        let f = Formula::atomic(get_var('a'));
        assert_eq!(f.interpret(&partial_valuation()), Formula::TruthValue(true));

        let f = Formula::atomic(get_var('b'));
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::TruthValue(false)
        );
    }

    #[test]
    fn unknown_atom() {
        let var = get_var('p');
        let f = Formula::atomic(var);
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::Atomic(Arc::new(var))
        );
    }

    #[test]
    fn conjunction_known() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('b'));
        let var3 = Arc::new(get_var('c'));
        let f: Formula<_> = var1.and(var2).and(var3);
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::TruthValue(false)
        );
    }

    #[test]
    fn conjunction_unknown_with_false() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('b'));
        let var3 = Arc::new(get_var('p'));
        let f: Formula<_> = var1.and(var2).and(var3);
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::TruthValue(false)
        );
    }

    #[test]
    fn conjunction_unknown_with_truths() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('d'));
        let var3 = Arc::new(get_var('p'));

        let f: Formula<_> = var1.clone().and(var2.clone()).and(var3.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::Atomic(var3.clone())
        );

        let f: Formula<_> = var2.clone().and(var3.clone()).and(var1.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::Atomic(var3.clone())
        );

        let f: Formula<_> = var2.and(var3.clone().and(var1));
        assert_eq!(f.interpret(&partial_valuation()), Formula::Atomic(var3));
    }

    #[test]
    fn conjunction_unknown_with_truths_and_negate() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('d'));
        let var3 = Arc::new(get_var('p'));

        let f: Formula<_> = var1.clone().and(var2.clone()).and(var3.clone().not());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::Atomic(var3.clone()).not()
        );

        let f: Formula<_> = var2.clone().and(var3.clone().not()).and(var1.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::Atomic(var3.clone()).not()
        );

        let f: Formula<_> = var2.and(var3.clone().not().and(var1));
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::Atomic(var3).not()
        );
    }

    #[test]
    fn disjunction_known() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('b'));
        let var3 = Arc::new(get_var('c'));
        let f: Formula<_> = var1.or(var2).or(var3);
        assert_eq!(f.interpret(&partial_valuation()), Formula::TruthValue(true));
    }

    #[test]
    fn disjunction_unknown_with_truth() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('b'));
        let var3 = Arc::new(get_var('p'));
        let f: Formula<_> = var1.or(var2).or(var3);
        assert_eq!(f.interpret(&partial_valuation()), Formula::TruthValue(true));
    }

    #[test]
    fn disjunction_unknown_with_falsitys() {
        let var1 = Arc::new(get_var('b'));
        let var2 = Arc::new(get_var('c'));
        let var3 = Arc::new(get_var('p'));

        let f: Formula<_> = var1.clone().or(var2.clone()).or(var3.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::Atomic(var3.clone())
        );

        let f: Formula<_> = var2.clone().or(var3.clone()).or(var1.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::Atomic(var3.clone())
        );

        let f: Formula<_> = var2.or(var3.clone().or(var1));
        assert_eq!(f.interpret(&partial_valuation()), Formula::Atomic(var3));
    }

    #[test]
    fn xor_known() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('b'));
        let f: Formula<_> = var1.xor(var2);
        assert_eq!(f.interpret(&partial_valuation()), Formula::TruthValue(true));
    }

    #[test]
    fn xor_unknown_with_different() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('b'));
        let var3 = Arc::new(get_var('p'));
        let f: Formula<_> = var1.xor(var2).xor(var3.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::Atomic(var3).not()
        );
    }

    #[test]
    fn xor_unknown_with_same() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('d'));
        let var3 = Arc::new(get_var('p'));
        let f: Formula<_> = var1.xor(var2).xor(var3.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::Atomic(var3.clone())
        );

        let var1 = Arc::new(get_var('b'));
        let var2 = Arc::new(get_var('c'));
        let f: Formula<_> = var1.xor(var2).xor(var3.clone());
        assert_eq!(f.interpret(&partial_valuation()), Formula::Atomic(var3));
    }

    #[test]
    fn imply_known() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('b'));
        let f: Formula<_> = var1.clone().implies(var2.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::TruthValue(false)
        );

        let f: Formula<_> = var2.implies(var1);
        assert_eq!(f.interpret(&partial_valuation()), Formula::TruthValue(true));
    }

    #[test]
    fn imply_unknown_from_false_vacuous() {
        let var1 = Arc::new(get_var('b'));
        let var2 = Arc::new(get_var('p'));
        let f: Formula<_> = var1.implies(var2);
        assert_eq!(f.interpret(&partial_valuation()), true.into());
    }

    #[test]
    fn imply_unknown_to_false_is_negation() {
        let var1 = Arc::new(get_var('b'));
        let var2 = Arc::new(get_var('p'));
        let f: Formula<_> = var2.clone().implies(var1);
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::Atomic(var2).not()
        );
    }

    #[test]
    fn imply_unknown_from_true_is_identity() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('p'));
        let f: Formula<_> = var1.implies(var2.clone());
        assert_eq!(f.interpret(&partial_valuation()), Formula::Atomic(var2));
    }

    #[test]
    fn imply_unknown_to_true_is_true() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('p'));
        let f: Formula<_> = var2.implies(var1);
        assert_eq!(f.interpret(&partial_valuation()), true.into());
    }

    #[test]
    fn eq_known_same() {
        let var1 = Arc::new(get_var('c'));
        let var2 = Arc::new(get_var('b'));
        let f: Formula<_> = var1.clone().equivalent(var2.clone());
        assert_eq!(f.interpret(&partial_valuation()), Formula::TruthValue(true));

        let f: Formula<_> = var2.equivalent(var1);
        assert_eq!(f.interpret(&partial_valuation()), true.into());
    }

    #[test]
    fn eq_known_different() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('b'));
        let f: Formula<_> = var1.clone().equivalent(var2.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::TruthValue(false)
        );

        let f: Formula<_> = var2.equivalent(var1);
        assert_eq!(f.interpret(&partial_valuation()), false.into());
    }

    #[test]
    fn eq_unknown_with_false_is_negation() {
        let var1 = Arc::new(get_var('b'));
        let var2 = Arc::new(get_var('p'));

        let f: Formula<_> = var1.clone().equivalent(var2.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::Atomic(var2.clone()).not()
        );

        let f: Formula<_> = var2.clone().equivalent(var1);
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::Atomic(var2).not()
        );
    }

    #[test]
    fn eq_unknown_with_false_is_identity() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('p'));

        let f: Formula<_> = var1.clone().equivalent(var2.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::Atomic(var2.clone())
        );

        let f: Formula<_> = var2.clone().equivalent(var1);
        assert_eq!(f.interpret(&partial_valuation()), Formula::Atomic(var2));
    }
}
