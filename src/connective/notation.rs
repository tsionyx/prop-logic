use std::{
    fmt::{self, Write},
    sync::Arc,
};

#[derive(Debug, Clone)]
/// Name or notation of the function.
pub enum FunctionNotation {
    /// Symbolic representation of a functions as an operator.
    /// The common usage of this symbol:
    /// - as [prefix](https://en.wikipedia.org/wiki/Polish_notation) operator form for unary functions;
    /// - as [infix](https://en.wikipedia.org/wiki/Infix_notation) operator form for binary functions.
    Symbol(char),

    /// The name of the function (usually as the latin name).
    Name(Arc<str>),
    // TODO: more categories like ASCII, Polish notation, scheme element name, etc.
}

impl From<char> for FunctionNotation {
    fn from(value: char) -> Self {
        Self::Symbol(value)
    }
}

impl From<String> for FunctionNotation {
    fn from(value: String) -> Self {
        Self::Name(value.into())
    }
}

impl From<&'static str> for FunctionNotation {
    fn from(value: &'static str) -> Self {
        Self::Name(value.into())
    }
}

impl fmt::Display for FunctionNotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Name(name) => f.write_str(name),
            Self::Symbol(s) => f.write_char(*s),
        }
    }
}
