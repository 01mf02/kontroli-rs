//! Common error type.

use alloc::string::String;

/// Common error type.
#[derive(Debug)]
pub enum Error {
    Parse,
    Scope(ScopeError),
    Signature(SignatureError),
    Symbols(SymbolsError),
    Typing(TypingError),
}

#[derive(Debug, Eq, PartialEq)]
pub enum ScopeError {
    UndeclaredSymbol(String),
    Underscore,
    NoPrepattern,
    NoTopPattern,
    PatternArguments,
}

#[derive(Debug)]
pub enum SignatureError {
    Reintroduction,
    NonRewritable,
}

#[derive(Debug)]
pub enum SymbolsError {
    Reinsertion,
}

#[derive(Debug)]
pub enum TypingError {
    ProductExpected,
    SortExpected,
    BindNoType,
    Unconvertible,
    KindNotTypable,
    UnexpectedKind,
    DomainFreeAbstraction,
    TypeAndTermEmpty,
    TypeNotFound,
}

impl<'a> From<nom::Err<nom::error::VerboseError<&'a [u8]>>> for Error {
    fn from(_: nom::Err<nom::error::VerboseError<&'a [u8]>>) -> Self {
        Self::Parse
    }
}

impl From<ScopeError> for Error {
    fn from(err: ScopeError) -> Self {
        Self::Scope(err)
    }
}

impl From<SignatureError> for Error {
    fn from(err: SignatureError) -> Self {
        Self::Signature(err)
    }
}

impl From<SymbolsError> for Error {
    fn from(err: SymbolsError) -> Self {
        Self::Symbols(err)
    }
}

impl From<TypingError> for Error {
    fn from(err: TypingError) -> Self {
        Self::Typing(err)
    }
}
