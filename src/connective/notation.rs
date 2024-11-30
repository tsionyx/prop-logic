use std::fmt::{self, Write};

use crate::utils::Str;

#[derive(Debug, Clone, PartialEq, Eq)]
/// Name or notation of the function.
pub enum FunctionNotation {
    /// Symbolic representation of a functions as an operator.
    /// The common usage of this symbol:
    /// - as [prefix](https://en.wikipedia.org/wiki/Polish_notation) operator form for unary functions;
    /// - as [infix](https://en.wikipedia.org/wiki/Infix_notation) operator form for binary functions.
    Symbolic(char),

    /// The same as [Symbolic][Self::Symbolic] but for ASCII characters.
    AsciiSymbolic(u8),

    /// ASCII symbolic representation for more than one character.
    SymbolicStr(Str),

    /// Prefix notation for a logical function.
    ///
    /// <https://en.wikipedia.org/wiki/J%C3%B3zef_Maria_Boche%C5%84ski#Pr%C3%A9cis_de_logique_math%C3%A9matique>
    Polish(char),

    /// The name of the logic gate corresponding to the function.
    ///
    /// <https://en.wikipedia.org/wiki/Logic_gate>
    SchemeGate(Str),

    /// Common language representation without any special formalized meaning.
    CommonName(Str),

    /// Special form for unary identity function where no special symbol is necessary.
    Empty,
}

impl FunctionNotation {
    /// Get symbolic representation if possible.
    pub fn get_symbol(&self) -> Option<char> {
        match self {
            Self::Symbolic(s) => Some(*s),
            Self::AsciiSymbolic(a) => Some(char::from(*a)),
            Self::SymbolicStr(_)
            | Self::Polish(_)
            | Self::SchemeGate(_)
            | Self::CommonName(_)
            | Self::Empty => None,
        }
    }

    pub(crate) fn symbolic_str(name: &'static str) -> Self {
        Self::SymbolicStr(name.into())
    }

    pub(crate) fn scheme_gate(name: &'static str) -> Self {
        Self::SchemeGate(name.into())
    }

    pub(crate) fn common(name: &'static str) -> Self {
        Self::CommonName(name.into())
    }
}

impl From<char> for FunctionNotation {
    fn from(value: char) -> Self {
        if value.is_ascii() {
            Self::AsciiSymbolic(value as u8)
        } else {
            Self::Symbolic(value)
        }
    }
}

impl fmt::Display for FunctionNotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Symbolic(s) => f.write_char(*s),
            Self::AsciiSymbolic(a) => f.write_char(char::from(*a)),
            Self::SymbolicStr(s) => f.write_str(s),
            Self::Polish(p) => write!(f, "{p}pq"),
            Self::SchemeGate(name) | Self::CommonName(name) => f.write_str(name),
            Self::Empty => Ok(()),
        }
    }
}
