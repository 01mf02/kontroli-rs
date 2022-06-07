//! Common error type.

use crate::gctx::Error as GCtxError;
use crate::kernel::Error as TypingError;
use crate::share::Error as ShareError;
use crate::symbols::Error as SymbolsError;

/// Common error type.
#[derive(Debug)]
pub enum Error {
    Share(ShareError),
    GCtx(GCtxError),
    Symbols(SymbolsError),
    Typing(TypingError),
}

impl From<ShareError> for Error {
    fn from(err: ShareError) -> Self {
        Self::Share(err)
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
