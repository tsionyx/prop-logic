use super::super::{
    equivalences::{apply_many, RewritingRuleDebug},
    Formula,
};

/// General shape for a [`Formula`]'s normal form.
pub trait NormalForm<T>: TryFrom<Formula<T>> + Into<Formula<T>> {
    /// Which rules to apply before converting
    /// a [`Formula`] to the normal form.
    fn rules<V: PartialEq + Clone>() -> Vec<Box<dyn RewritingRuleDebug<V>>>;

    /// Convert a [`Formula`] using the list
    /// of [transformation rules][Self::rules].
    fn prepare<V>(formula: Formula<V>) -> Formula<V>
    where
        V: PartialEq + Clone + 'static,
    {
        let rules = Self::rules();
        let rules: Vec<_> = rules.iter().map(|x| x.as_ref().up()).collect();
        apply_many(formula, rules.as_slice(), true)
    }
}
