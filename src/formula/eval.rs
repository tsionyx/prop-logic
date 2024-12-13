use std::{borrow::Borrow, collections::HashMap as Map, hash::Hash, sync::Arc};

use crate::connective::{Evaluable, Reducible as _};

use super::{atom::Assignment, connective::AnyConnective, formula::Formula};

#[derive(Debug)]
/// Mapping the [`Atoms`]s of a [`Formula`]
/// to one of the [truth values](https://en.wikipedia.org/wiki/Truth_value).
///
/// <https://en.wikipedia.org/wiki/Valuation_(logic)>
pub struct Valuation<T> {
    values: Map<Arc<T>, Assignment>,
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
        self.values.get(key).and_then(Assignment::get).copied()
    }

    /// Set a truth value to a specific [`Atom`].
    pub fn assign(&mut self, key: Arc<T>, value: bool) {
        let _previous_value = self.values.insert(key, Assignment::Value(value));
    }
}

impl<T> Evaluable for Formula<T> {
    type Partial = Self;

    fn terminal(value: bool) -> Self {
        Self::TruthValue(value)
    }

    fn partial(val: Self) -> Self {
        val
    }

    fn into_terminal(self) -> Result<bool, Self> {
        if let Self::TruthValue(value) = self {
            Ok(value)
        } else {
            Err(self)
        }
    }
}

impl<T> Formula<T>
where
    T: Eq + Hash, // for the `Valuation::get_assignment`
{
    #[must_use]
    /// Trying to get the value of [`Formula`]
    /// by reducing it using available [Atom]'s [`Valuation`].
    ///
    /// If the [`Valuation`] is incomplete,
    /// the reduced [`Formula`] is going to be produced
    /// by doing short-circuit calculation wherever possible.
    pub fn interpret(&self, i12n: &Valuation<T>) -> Self {
        self.try_reduce(i12n)
    }

    fn try_reduce(&self, i12n: &Valuation<T>) -> Self {
        if let Self::Atomic(p) = self {
            return i12n
                .get_assignment(p.as_ref())
                .map_or_else(|| self.clone(), Self::terminal);
        }

        let conn = self.get_connective();
        match &conn {
            AnyConnective::Nullary(operator) => operator
                .connective
                .try_reduce([])
                .ok()
                .expect("The nullary operator always reducible"),
            AnyConnective::Unary(conn) => {
                let operator = &conn.connective;
                let [operand] = &conn.operands;

                let reduced = operand.try_reduce(i12n);
                operator.try_reduce([reduced]).unwrap_or_else(|[f]| {
                    match f.into_partial() {
                        Ok(f) => operator.compose([f]),
                        Err(val) => {
                            // should be unreachable
                            Self::terminal(operator.as_ref().eval([val]))
                        }
                    }
                })
            }
            AnyConnective::Binary(conn) => {
                let operator = &conn.connective;
                let [op1, op2] = &conn.operands;

                let reduced = [op1.try_reduce(i12n), op2.try_reduce(i12n)];
                operator.try_reduce(reduced).unwrap_or_else(|[f1, f2]| {
                    match (f1.into_partial(), f2.into_partial()) {
                        (Ok(f1), Ok(f2)) => operator.compose([f1, f2]),
                        (Ok(f), Err(val)) => {
                            // unreachable!("This branch should be catched by `operator.try_reduce`")
                            operator.compose([f, Self::TruthValue(val)])
                        }
                        (Err(val), Ok(f)) => {
                            // unreachable!("This branch should be catched by `operator.try_reduce`")
                            operator.compose([Self::TruthValue(val), f])
                        }
                        (Err(val1), Err(val2)) => {
                            // should be unreachable
                            Self::terminal(operator.as_ref().eval([val1, val2]))
                        }
                    }
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicU64, Ordering};

    use super::{
        super::{
            ops::{And, Equivalent, Implies, Not, Or, Xor},
            Variable,
        },
        *,
    };

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
            !Formula::Atomic(var3.clone())
        );

        let f: Formula<_> = var2.clone().and(var3.clone().not()).and(var1.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            !Formula::Atomic(var3.clone())
        );

        let f: Formula<_> = var2.and(var3.clone().not().and(var1));
        assert_eq!(f.interpret(&partial_valuation()), !Formula::Atomic(var3));
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
        assert_eq!(f.interpret(&partial_valuation()), !Formula::Atomic(var3),);
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
        assert_eq!(f.interpret(&partial_valuation()), !Formula::Atomic(var2));
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
            !Formula::Atomic(var2.clone()),
        );

        let f: Formula<_> = var2.clone().equivalent(var1);
        assert_eq!(f.interpret(&partial_valuation()), !Formula::Atomic(var2),);
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
