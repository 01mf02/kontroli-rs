//! Rewrite patterns.

use crate::application::format as fmt_appl;
use alloc::vec::Vec;
use core::convert::TryFrom;
use core::fmt::{self, Display};

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
#[derive(Copy, Clone, Debug)]
pub struct Miller(usize);

impl From<usize> for Miller {
    fn from(u: usize) -> Self {
        Self(u)
    }
}

impl From<Miller> for usize {
    fn from(m: Miller) -> Self {
        m.0
    }
}

impl Display for Miller {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "μ{}", self.0)
    }
}

/// Rewrite pattern.
///
/// This may be nonlinear; e.g. `eq X X` is a valid pattern.
#[derive(Clone)]
pub enum Pattern<S, V> {
    /// matches an application
    Symb(S, Vec<Pattern<S, V>>),
    /// matches any term, variable may appear multiple times in
    /// both left-hand and right-hand sides of rewrite rule
    Var(V),
}

/// Pattern at the left-hand side of a rewrite rule.
///
/// The top pattern of a rule must be an application of patterns to a symbol.
/// This is to exclude rules matching any term, such as `[X] X --> f`.
pub type TopPattern<S, V> = crate::Application<S, Pattern<S, V>>;

impl<S, V> Pattern<S, V> {
    pub fn map_vars<W>(self, f: &mut impl FnMut(V) -> W) -> Pattern<S, W> {
        use Pattern::*;
        match self {
            Symb(s, args) => Symb(s, args.into_iter().map(|a| a.map_vars(f)).collect()),
            Var(v) => Var(f(v)),
        }
    }
}

impl<S, V> From<TopPattern<S, V>> for Pattern<S, V> {
    fn from(tp: TopPattern<S, V>) -> Self {
        Self::Symb(tp.symbol, tp.args)
    }
}

impl<S, V> TryFrom<Pattern<S, V>> for TopPattern<S, V> {
    type Error = ();

    fn try_from(p: Pattern<S, V>) -> Result<Self, Self::Error> {
        match p {
            Pattern::Symb(symbol, args) => Ok(TopPattern { symbol, args }),
            _ => Err(()),
        }
    }
}

impl<S: Display, V: Display> Display for Pattern<S, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Symb(s, pats) => fmt_appl(s, pats, f),
            Self::Var(m) => m.fmt(f),
        }
    }
}
