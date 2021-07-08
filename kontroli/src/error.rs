//! Common error type.

use crate::parse;
use alloc::string::String;

/// Common error type.
#[derive(Debug)]
pub enum Error {
    Parse(parse::Error),
    Scope(ScopeError),
    Signature(SignatureError),
    Symbols(SymbolsError),
    Typing(TypingError),
}

#[derive(Debug, Eq, PartialEq)]
pub enum ScopeError {
    UndeclaredSymbol(String),
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

impl From<parse::Error> for Error {
    fn from(err: parse::Error) -> Self {
        Self::Parse(err)
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
