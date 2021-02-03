use alloc::vec::Vec;
use core::fmt::{self, Display};

/// Application of a list of arguments to a symbol.
#[derive(Clone)]
pub struct Application<S, A> {
    pub symbol: S,
    pub args: Vec<A>,
}

impl<S, A> Application<S, A> {
    pub fn map<F, B>(self, f: F) -> Application<S, B>
    where
        F: Fn(A) -> B,
    {
        Application {
            symbol: self.symbol,
            args: self.args.into_iter().map(f).collect(),
        }
    }
}

impl<S, A> From<S> for Application<S, A> {
    fn from(symbol: S) -> Self {
        let args = Vec::new();
        Self { symbol, args }
    }
}

impl<S: Display, A: Display> Display for Application<S, A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        format(&self.symbol, &self.args, f)
    }
}

pub fn format<H: Display, T: Display>(head: &H, tail: &[T], f: &mut fmt::Formatter) -> fmt::Result {
    let parens = !tail.is_empty();
    if parens {
        write!(f, "(")?;
    };
    write!(f, "{}", head)?;
    for t in tail {
        write!(f, " {}", t)?;
    }
    if parens {
        write!(f, ")")?;
    };
    Ok(())
}
