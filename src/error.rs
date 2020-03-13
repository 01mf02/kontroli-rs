use crate::{rule, scope, signature, typing};
use std::{fmt, io};

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Rule(rule::Error),
    Scope(scope::Error),
    Signature(signature::Error),
    Typing(typing::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::Io(ref err) => err.fmt(f),
            Self::Typing(ref err) => err.fmt(f),
            Self::Scope(ref err) => err.fmt(f),
            Self::Rule(ref err) => err.fmt(f),
            Self::Signature(ref err) => err.fmt(f),
        }
    }
}

impl std::error::Error for Error {}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<typing::Error> for Error {
    fn from(err: typing::Error) -> Self {
        Self::Typing(err)
    }
}

impl From<scope::Error> for Error {
    fn from(err: scope::Error) -> Self {
        Self::Scope(err)
    }
}

impl From<rule::Error> for Error {
    fn from(err: rule::Error) -> Self {
        Self::Rule(err)
    }
}

impl From<signature::Error> for Error {
    fn from(err: signature::Error) -> Self {
        Self::Signature(err)
    }
}
