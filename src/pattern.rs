//! Rewrite patterns à la Miller.

use crate::scope::Error;
use crate::symbol::Symbol;
use crate::term::{fmt_appl, Term};
use std::convert::TryFrom;
use std::fmt;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Miller(pub usize);

impl fmt::Display for Miller {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "μ{}", self.0)
    }
}

#[derive(Clone)]
pub enum Pattern {
    MVar(Miller),
    Symb(Symbol, Vec<Pattern>),
    Joker,
}

#[derive(Clone)]
pub struct TopPattern {
    pub symbol: Symbol,
    pub args: Vec<Pattern>,
}

impl From<Symbol> for TopPattern {
    fn from(symbol: Symbol) -> Self {
        let args = Vec::new();
        Self { symbol, args }
    }
}

impl From<TopPattern> for Pattern {
    fn from(tp: TopPattern) -> Self {
        Self::Symb(tp.symbol, tp.args)
    }
}

impl TryFrom<Pattern> for TopPattern {
    type Error = Error;

    fn try_from(p: Pattern) -> Result<Self, Self::Error> {
        match p {
            Pattern::Symb(symbol, args) => Ok(TopPattern { symbol, args }),
            _ => Err(Error::NoTopPattern),
        }
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Symb(s, pats) => fmt_appl(&Term::Symb(s.clone()), pats, f),
            Self::MVar(m) => m.fmt(f),
            Self::Joker => write!(f, "_"),
        }
    }
}
