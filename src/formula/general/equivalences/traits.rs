use super::super::formula::Formula;

/// Represent the logical law to convert a [`Formula`]
/// to the equivalent one.
pub trait RewritingRule<T> {
    /// Apply the rule recursively.
    ///
    /// Specifying the `minimize` flag ensures the [`Formula`]'s size will not increase,
    /// i.e. the rule will not be applied if it creates longer [`Formula`].
    fn apply_all(&self, formula: Formula<T>, minimize: bool) -> Formula<T> {
        let formula = formula.map(|f| self.apply_all(f, minimize));
        self.apply(formula, minimize)
    }

    /// Apply the rule returning the (possibly unchanged) new [`Formula`].
    ///
    /// Specifying the `minimize` flag ensures the [`Formula`]'s size will not increase,
    /// i.e. the rule will not be applied if it creates longer [`Formula`].
    fn apply(&self, formula: Formula<T>, minimize: bool) -> Formula<T> {
        if minimize {
            self.minimize(formula)
        } else {
            self.reduce(formula)
        }
        .unwrap_or_else(|original| original)
    }

    /// Try to apply the rule to the [`Formula`], if applicable.
    ///
    /// The rule is applicable if the necessary structure is found in the [`Formula`].
    /// For only-shortening rules, the implementation should be the same
    /// as for the [`minimize`][Self::minimize].
    ///
    /// # Errors
    /// If the rule cannot be applied for the given [`Formula`]
    /// return the [`Err`] variant containing original [`Formula`].
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        self.minimize(formula)
    }

    /// Try to apply the rule to the [`Formula`], if applicable.
    ///
    /// The rule is applicable if the necessary structure is found,
    /// and if the rule will not introduce more connective,
    /// but rather reduce the original ones.
    ///
    /// For example, the [De Morgan's laws](https://en.wikipedia.org/wiki/De_Morgan%27s_laws)
    /// cannot minimize the [`Formula`] in the general case, but convert it to different form.
    ///
    /// # Errors
    /// If the rule cannot be applied for the given [`Formula`]
    /// return the [`Err`] variant containing original [`Formula`].
    fn minimize(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>>;
}
