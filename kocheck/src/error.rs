use kontroli as ko;
use std::io;

/// Central error type.
#[derive(Debug)]
pub enum Error {
    Module,
    Io(io::Error),
    Ko(ko::Error),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<ko::Error> for Error {
    fn from(err: ko::Error) -> Self {
        Self::Ko(err)
    }
}
