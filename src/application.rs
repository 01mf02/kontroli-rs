use crate::fmt::application as fmt_appl;
use alloc::vec::Vec;
use core::fmt::{self, Display};

/// Application of a list of arguments to a symbol.
#[derive(Clone)]
pub struct Application<S, A> {
    pub symbol: S,
    pub args: Vec<A>,
}

impl<S, A> From<S> for Application<S, A> {
    fn from(symbol: S) -> Self {
        let args = Vec::new();
        Self { symbol, args }
    }
}

impl<S: Display, A: Display> Display for Application<S, A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_appl(&self.symbol, &self.args, f)
    }
}
