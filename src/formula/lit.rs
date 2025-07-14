use std::{
    fmt::{self, Display, Write as _},
    ops::Not,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
/// A [literal][Literal] is a kind of generalization of
/// a propositional variable that can be either:
/// - in plain form, or just
///   the variable's [identity][crate::connective::LogicalIdentity];
/// - in negated form, or
///   the variable's [negation][crate::connective::Negation]
///
/// <https://en.wikipedia.org/wiki/Literal_(mathematical_logic)>
pub enum Literal<T> {
    /// The negated value.
    Neg(T),

    /// The value itself.
    Pos(T),
}

impl<T> From<T> for Literal<T> {
    fn from(value: T) -> Self {
        Self::Pos(value)
    }
}

impl<T> Literal<T> {
    /// Get a referenced [`Literal`] value.
    pub const fn as_ref(&self) -> Literal<&T> {
        match self {
            Self::Neg(n) => Literal::Neg(n),
            Self::Pos(p) => Literal::Pos(p),
        }
    }

    /// Get the underlying variable, ignoring the polarity.
    pub const fn as_var(&self) -> &T {
        match self {
            Self::Neg(p) | Self::Pos(p) => p,
        }
    }

    /// Get the sign of the [`Literal`].
    pub const fn polarity(&self) -> bool {
        match self {
            Self::Neg(_) => false,
            Self::Pos(_) => true,
        }
    }
}

impl<T> Not for Literal<T> {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Neg(var) => Self::Pos(var),
            Self::Pos(var) => Self::Neg(var),
        }
    }
}

impl<T: Display> Display for Literal<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Neg(n) => {
                f.write_char('-')?;
                Display::fmt(n, f)
            }
            Self::Pos(p) => {
                f.write_char('+')?;
                Display::fmt(p, f)
            }
        }
    }
}

mod impls {
    use super::{super::Formula, Literal};

    impl<T> Formula<T> {
        /// Represent a [`Formula`] as a single [`Literal`] variable, if possible.
        pub fn as_literal(&self) -> Option<Literal<&T>> {
            match self {
                Self::Atomic(p) => Some(Literal::Pos(p)),
                // The following (simpler) recursive defintion implies double negation elimination
                // and therefore formally not correct since during conversion back to `Formula`
                // the information on both of negations will be lost:
                //Self::Not(f) => f.as_literal().map(|lit| !lit),
                Self::Not(n) => {
                    if let Self::Atomic(x) = n.as_ref() {
                        Some(Literal::Neg(x))
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
    }

    impl<T> From<Literal<T>> for Formula<T> {
        fn from(value: Literal<T>) -> Self {
            match value {
                Literal::Pos(x) => Self::atom(x),
                Literal::Neg(x) => !Self::atom(x),
            }
        }
    }

    impl<T> TryFrom<Formula<T>> for Literal<T> {
        type Error = Formula<T>;

        fn try_from(f: Formula<T>) -> Result<Self, Self::Error> {
            match f {
                Formula::Atomic(p) => Ok(Self::Pos(p)),
                // The following (simpler) recursive defintion implies double negation elimination
                // and therefore formally not correct since during conversion back to `Formula`
                // the information on both of negations will be lost:
                // Formula::Not(f) => Self::try_from(*f).map(|lit| !lit).map_err(|f| !f),
                Formula::Not(n) => {
                    if let Formula::Atomic(x) = *n {
                        Ok(Self::Neg(x))
                    } else {
                        Err(Formula::Not(n))
                    }
                }
                other => Err(other),
            }
        }
    }
}
