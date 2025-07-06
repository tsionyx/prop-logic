use super::super::AnyConnective;

#[derive(Debug, thiserror::Error)]
/// The error while converting to/from normal forms.
pub enum Error {
    #[error("Found negation in unexpected position (for {connective:?}) for {normal_form:?}")]
    /// Found bad connective under negation while
    /// transforming into given normal form.
    UnexpectedNot {
        /// The name of normal form type.
        normal_form: &'static str,
        /// The name of the connective found negating.
        connective: AnyConnective<(), ()>,
    },

    #[error("Found unexpected connective {connective:?} for {normal_form:?}")]
    /// Found bad connective while
    /// transforming into given normal form.
    InvalidConnective {
        /// The name of normal form type.
        normal_form: &'static str,
        /// The name of the connective found.
        connective: AnyConnective<(), ()>,
    },
}
