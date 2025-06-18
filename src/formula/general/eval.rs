use std::{borrow::Borrow, hash::Hash};

use crate::connective::{Evaluable, TruthFn as _};

use super::{super::eval::Valuation, connective::AnyConnective, formula::Formula};

impl<T> Evaluable for Formula<T> {
    type Partial = Self;

    fn terminal(value: bool) -> Self {
        Self::truth(value)
    }

    fn is_tautology(&self) -> bool {
        matches!(self, Self::TruthValue(x) if *x)
    }

    fn is_contradiction(&self) -> bool {
        matches!(self, Self::TruthValue(x) if !*x)
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
    T: Eq + Hash + Clone, // for the `Valuation::get_assignment` and `Formula::clone`.
{
    #[must_use]
    /// Trying to get the value of [`Formula`]
    /// by reducing it using available variables' [`Valuation`].
    ///
    /// If the [`Valuation`] is incomplete,
    /// the reduced [`Formula`] is going to be produced
    /// by doing short-circuit calculation wherever possible.
    pub fn interpret<K>(&self, i12n: &Valuation<K>) -> Self
    where
        K: Borrow<T> + Eq + Hash, // for the `Valuation::get_assignment`
    {
        self.try_reduce(i12n)
    }

    fn try_reduce<K>(&self, i12n: &Valuation<K>) -> Self
    where
        K: Borrow<T> + Eq + Hash, // for the `Valuation::get_assignment`
    {
        if let Self::Atomic(p) = self {
            return i12n
                .get_assignment(p)
                .map_or_else(|| self.clone(), Self::terminal);
        }

        let conn = self.get_connective();
        match &conn {
            AnyConnective::Nullary(operator) => operator.connective.eval([]),
            AnyConnective::Unary(conn) => {
                let operator = &conn.connective;
                let [operand] = &conn.operands;
                let reduced = operand.try_reduce(i12n);
                operator.eval([reduced])
            }
            AnyConnective::Binary(conn) => {
                let operator = &conn.connective;
                let [op1, op2] = &conn.operands;
                let reduced = [op1.try_reduce(i12n), op2.try_reduce(i12n)];
                operator.eval(reduced)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicU64, Ordering};

    use super::{
        super::super::{
            ops::{And as _, Equivalent as _, Implies as _, Not as _, Or as _, Xor as _},
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

    // emulate std::sync::Arc
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    struct Arc<T>(T);

    impl<T> Arc<T> {
        const fn new(x: T) -> Self {
            Self(x)
        }
    }

    fn partial_valuation() -> Valuation<Arc<Variable<char>>> {
        let mut val = Valuation::empty();
        val.assign(Arc::new(get_var('a')), true);
        val.assign(Arc::new(get_var('b')), false);
        val.assign(Arc::new(get_var('c')), false);
        val.assign(Arc::new(get_var('d')), true);
        val
    }

    #[test]
    fn tautology() {
        let f: Formula<Arc<Variable<char>>> = Formula::tautology();
        assert_eq!(f.interpret(&partial_valuation()), Formula::truth(true));
    }

    #[test]
    fn contradiction() {
        let f: Formula<Arc<Variable<char>>> = Formula::contradiction();
        assert_eq!(f.interpret(&partial_valuation()), Formula::truth(false));
    }

    #[test]
    fn known_var() {
        let f = Formula::atom(Arc::new(get_var('a')));
        assert_eq!(f.interpret(&partial_valuation()), Formula::truth(true));

        let f = Formula::atom(Arc::new(get_var('b')));
        assert_eq!(f.interpret(&partial_valuation()), Formula::truth(false));
    }

    #[test]
    fn unknown_var() {
        let var = get_var('p');
        let f = Formula::atom(Arc::new(var));
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::atom(Arc::new(var))
        );
    }

    #[test]
    fn conjunction_known() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('b'));
        let var3 = Arc::new(get_var('c'));
        let f = Formula::from(var1).and(var2).and(var3);
        assert_eq!(f.interpret(&partial_valuation()), Formula::truth(false));
    }

    #[test]
    fn conjunction_unknown_with_false() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('b'));
        let var3 = Arc::new(get_var('p'));
        let f = Formula::from(var1).and(var2).and(var3);
        assert_eq!(f.interpret(&partial_valuation()), Formula::truth(false));
    }

    #[test]
    fn conjunction_unknown_with_truths() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('d'));
        let var3 = Arc::new(get_var('p'));

        let f = Formula::from(var1.clone())
            .and(var2.clone())
            .and(var3.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::atom(var3.clone())
        );

        let f = Formula::from(var2.clone())
            .and(var3.clone())
            .and(var1.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::atom(var3.clone())
        );

        let f = Formula::from(var2).and(Formula::from(var3.clone()).and(var1));
        assert_eq!(f.interpret(&partial_valuation()), Formula::atom(var3));
    }

    #[test]
    fn conjunction_unknown_with_truths_and_negate() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('d'));
        let var3 = Arc::new(get_var('p'));

        let f = Formula::from(var1.clone())
            .and(var2.clone())
            .and(Formula::from(var3.clone()).not());
        assert_eq!(
            f.interpret(&partial_valuation()),
            !Formula::atom(var3.clone())
        );

        let f = Formula::from(var2.clone())
            .and(Formula::from(var3.clone()).not())
            .and(var1.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            !Formula::atom(var3.clone())
        );

        let f = Formula::from(var2).and(Formula::from(var3.clone()).not().and(var1));
        assert_eq!(f.interpret(&partial_valuation()), !Formula::atom(var3));
    }

    #[test]
    fn disjunction_known() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('b'));
        let var3 = Arc::new(get_var('c'));
        let f = Formula::from(var1).or(var2).or(var3);
        assert_eq!(f.interpret(&partial_valuation()), Formula::truth(true));
    }

    #[test]
    fn disjunction_unknown_with_truth() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('b'));
        let var3 = Arc::new(get_var('p'));
        let f = Formula::from(var1).or(var2).or(var3);
        assert_eq!(f.interpret(&partial_valuation()), Formula::truth(true));
    }

    #[test]
    fn disjunction_unknown_with_falsitys() {
        let var1 = Arc::new(get_var('b'));
        let var2 = Arc::new(get_var('c'));
        let var3 = Arc::new(get_var('p'));

        let f = Formula::from(var1.clone())
            .or(var2.clone())
            .or(var3.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::atom(var3.clone())
        );

        let f = Formula::from(var2.clone())
            .or(var3.clone())
            .or(var1.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::atom(var3.clone())
        );

        let f = Formula::from(var2).or(Formula::from(var3.clone()).or(var1));
        assert_eq!(f.interpret(&partial_valuation()), Formula::atom(var3));
    }

    #[test]
    fn xor_known() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('b'));
        let f = Formula::from(var1).xor(var2);
        assert_eq!(f.interpret(&partial_valuation()), Formula::truth(true));
    }

    #[test]
    fn xor_unknown_with_different() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('b'));
        let var3 = Arc::new(get_var('p'));
        let f = Formula::from(var1).xor(var2).xor(var3.clone());
        assert_eq!(f.interpret(&partial_valuation()), !Formula::atom(var3));
    }

    #[test]
    fn xor_unknown_with_same() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('d'));
        let var3 = Arc::new(get_var('p'));
        let f = Formula::from(var1).xor(var2).xor(var3.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::atom(var3.clone())
        );

        let var1 = Arc::new(get_var('b'));
        let var2 = Arc::new(get_var('c'));
        let f = Formula::from(var1).xor(var2).xor(var3.clone());
        assert_eq!(f.interpret(&partial_valuation()), Formula::atom(var3));
    }

    #[test]
    fn imply_known() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('b'));
        let f = Formula::from(var1.clone()).implies(var2.clone());
        assert_eq!(f.interpret(&partial_valuation()), Formula::truth(false));

        let f = Formula::from(var2).implies(var1);
        assert_eq!(f.interpret(&partial_valuation()), Formula::truth(true));
    }

    #[test]
    fn imply_unknown_from_false_vacuous() {
        let var1 = Arc::new(get_var('b'));
        let var2 = Arc::new(get_var('p'));
        let f = Formula::from(var1).implies(var2);
        assert_eq!(f.interpret(&partial_valuation()), Formula::truth(true));
    }

    #[test]
    fn imply_unknown_to_false_is_negation() {
        let var1 = Arc::new(get_var('b'));
        let var2 = Arc::new(get_var('p'));
        let f = Formula::from(var2.clone()).implies(var1);
        assert_eq!(f.interpret(&partial_valuation()), !Formula::atom(var2));
    }

    #[test]
    fn imply_unknown_from_true_is_identity() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('p'));
        let f = Formula::from(var1).implies(var2.clone());
        assert_eq!(f.interpret(&partial_valuation()), Formula::atom(var2));
    }

    #[test]
    fn imply_unknown_to_true_is_true() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('p'));
        let f = Formula::from(var2).implies(var1);
        assert_eq!(f.interpret(&partial_valuation()), Formula::truth(true));
    }

    #[test]
    fn eq_known_same() {
        let var1 = Arc::new(get_var('c'));
        let var2 = Arc::new(get_var('b'));
        let f = Formula::from(var1.clone()).equivalent(var2.clone());
        assert_eq!(f.interpret(&partial_valuation()), Formula::truth(true));

        let f = Formula::from(var2).equivalent(var1);
        assert_eq!(f.interpret(&partial_valuation()), Formula::truth(true));
    }

    #[test]
    fn eq_known_different() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('b'));
        let f = Formula::from(var1.clone()).equivalent(var2.clone());
        assert_eq!(f.interpret(&partial_valuation()), Formula::truth(false));

        let f = Formula::from(var2).equivalent(var1);
        assert_eq!(f.interpret(&partial_valuation()), Formula::truth(false));
    }

    #[test]
    fn eq_unknown_with_false_is_negation() {
        let var1 = Arc::new(get_var('b'));
        let var2 = Arc::new(get_var('p'));

        let f = Formula::from(var1.clone()).equivalent(var2.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            !Formula::atom(var2.clone()),
        );

        let f = Formula::from(var2.clone()).equivalent(var1);
        assert_eq!(f.interpret(&partial_valuation()), !Formula::atom(var2));
    }

    #[test]
    fn eq_unknown_with_false_is_identity() {
        let var1 = Arc::new(get_var('a'));
        let var2 = Arc::new(get_var('p'));

        let f = Formula::from(var1.clone()).equivalent(var2.clone());
        assert_eq!(
            f.interpret(&partial_valuation()),
            Formula::atom(var2.clone())
        );

        let f = Formula::from(var2.clone()).equivalent(var1);
        assert_eq!(f.interpret(&partial_valuation()), Formula::atom(var2));
    }
}
