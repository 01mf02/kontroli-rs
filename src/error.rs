//! Common error type.

use crate::{scope, signature, typing};
use alloc::string::String;
use core::fmt;

/// Common error type.
#[derive(Debug)]
pub enum Error {
    Parse(String),
    Scope(scope::Error),
    Signature(signature::Error),
    Typing(typing::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::Parse(ref err) => err.fmt(f),
            Self::Scope(ref err) => err.fmt(f),
            Self::Signature(ref err) => err.fmt(f),
            Self::Typing(ref err) => err.fmt(f),
        }
    }
}

impl<'a> From<nom::Err<nom::error::VerboseError<&'a [u8]>>> for Error {
    fn from(err: nom::Err<nom::error::VerboseError<&'a [u8]>>) -> Self {
        Self::Parse("TODO".to_string())
    }
}

impl From<scope::Error> for Error {
    fn from(err: scope::Error) -> Self {
        Self::Scope(err)
    }
}

impl From<signature::Error> for Error {
    fn from(err: signature::Error) -> Self {
        Self::Signature(err)
    }
}

impl From<typing::Error> for Error {
    fn from(err: typing::Error) -> Self {
        Self::Typing(err)
    }
}
