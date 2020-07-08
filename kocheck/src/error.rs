use kontroli::error::Error as KoError;
use std::io;

/// Central error type.
#[derive(Debug)]
pub enum Error {
    Module,
    Io(io::Error),
    Ko(KoError),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<KoError> for Error {
    fn from(err: KoError) -> Self {
        Self::Ko(err)
    }
}
