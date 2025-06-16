use super::super::formula::Formula;

/// Represent the logical law to convert a [`Formula`]
/// to the equivalent one.
///
/// <https://en.wikipedia.org/wiki/Rewriting#Logic>
pub trait RewritingRule<T> {
    /// Apply the rule recursively.
    ///
    /// Specifying the `allow_extend` flag enables the [`Formula`]'s
    /// to increase during the transformations,
    /// i.e. the rule will be applied even if it creates longer [`Formula`].
    fn apply_all(&self, formula: Formula<T>, allow_extend: bool) -> Formula<T> {
        let formula = if formula.is_complex() {
            formula.map(|f| self.apply_all(f, allow_extend))
        } else {
            formula
        };

        self.apply(formula, allow_extend)
    }

    /// Apply the rule returning the (possibly unchanged) new [`Formula`].
    ///
    /// Specifying the `allow_extend` flag enables the [`Formula`]'s
    /// to increase during the transformations,
    /// i.e. the rule will be applied even if it creates longer [`Formula`].
    fn apply(&self, formula: Formula<T>, allow_extend: bool) -> Formula<T> {
        if allow_extend {
            self.transform(formula)
        } else {
            self.reduce(formula)
        }
        .unwrap_or_else(|original| original)
    }

    /// Try to apply the rule to the [`Formula`], if applicable.
    ///
    /// The rule is applicable if the necessary structure is found in the [`Formula`].
    /// For only-shortening rules, the implementation should be the same
    /// as for the [`reduce`][Self::reduce].
    ///
    /// # Errors
    /// If the rule cannot be applied for the given [`Formula`]
    /// return the [`Err`] variant containing original [`Formula`].
    fn transform(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>> {
        self.reduce(formula)
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
    fn reduce(&self, formula: Formula<T>) -> Result<Formula<T>, Formula<T>>;
}
