//! Shared rewrite patterns.

use super::{Symbol, Term};
use crate::fmt::application as fmt_appl;
use alloc::vec::Vec;
use core::{convert::TryFrom, fmt};

/// Miller variable.
///
/// This refers to the variables appearing in a rewrite pattern
/// bound by the rewrite rule's context.
/// Historically, this type was created to distinguish variables bound
/// by the rule context and
/// by lambda abstraction, as occurring in higher-order patterns.
/// Because Kontroli abandoned higher-order patterns,
/// this distinction is no longer necessary; however, the name remains as
/// a tribute to Miller's contributions to higher-order rewriting.
///
/// Reference:
/// Dale Miller:
/// A Logic Programming Language with Lambda-Abstraction,
/// Function Variables, and Simple Unification.
/// J. Log. Comput. 1(4): 497-536 (1991).
/// doi: [10.1093/logcom/1.4.497](https://doi.org/10.1093/logcom/1.4.497)
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Miller(pub usize);

impl fmt::Display for Miller {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "μ{}", self.0)
    }
}

/// Shared rewrite pattern.
///
/// This may be nonlinear; e.g. `eq X X` is a valid pattern.
#[derive(Clone)]
pub enum Pattern<'s> {
    /// matches an application
    Symb(Symbol<'s>, Vec<Pattern<'s>>),
    /// matches any term, variable may appear multiple times in
    /// both left-hand and right-hand sides of rewrite rule
    MVar(Miller),
    /// matches any term
    Joker,
}

/// Pattern at the left-hand side of a rewrite rule.
///
/// The top pattern of a rule must be an application of patterns to a symbol.
/// This is to exclude rules matching any term, such as `[X] X --> f`.
#[derive(Clone)]
pub struct TopPattern<'s> {
    pub symbol: Symbol<'s>,
    pub args: Vec<Pattern<'s>>,
}

impl<'s> From<Symbol<'s>> for TopPattern<'s> {
    fn from(symbol: Symbol<'s>) -> Self {
        let args = Vec::new();
        Self { symbol, args }
    }
}

impl<'s> From<TopPattern<'s>> for Pattern<'s> {
    fn from(tp: TopPattern<'s>) -> Self {
        Self::Symb(tp.symbol, tp.args)
    }
}

impl<'s> TryFrom<Pattern<'s>> for TopPattern<'s> {
    type Error = ();

    fn try_from(p: Pattern<'s>) -> Result<Self, Self::Error> {
        match p {
            Pattern::Symb(symbol, args) => Ok(TopPattern { symbol, args }),
            _ => Err(()),
        }
    }
}

impl<'s> fmt::Display for Pattern<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Symb(s, pats) => fmt_appl(&Term::Symb(*s), pats, f),
            Self::MVar(m) => m.fmt(f),
            Self::Joker => write!(f, "_"),
        }
    }
}
