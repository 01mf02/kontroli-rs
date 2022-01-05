//! Common error type.

use alloc::string::String;

/// Common error type.
#[derive(Debug)]
pub enum Error {
    Scope(ScopeError),
    GCtx(GCtxError),
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
pub enum GCtxError {
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

impl From<ScopeError> for Error {
    fn from(err: ScopeError) -> Self {
        Self::Scope(err)
    }
}

impl From<GCtxError> for Error {
    fn from(err: GCtxError) -> Self {
        Self::GCtx(err)
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
