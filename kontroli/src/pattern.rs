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
pub type Miller = usize;

/// Rewrite pattern.
///
/// This may be nonlinear; e.g. `eq X X` is a valid pattern.
#[derive(Clone)]
pub enum Pattern<S> {
    /// matches an application
    Symb(S, Vec<Pattern<S>>),
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
pub type TopPattern<S> = crate::Application<S, Pattern<S>>;

impl<S> From<TopPattern<S>> for Pattern<S> {
    fn from(tp: TopPattern<S>) -> Self {
        Self::Symb(tp.symbol, tp.args)
    }
}

impl<S> TryFrom<Pattern<S>> for TopPattern<S> {
    type Error = ();

    fn try_from(p: Pattern<S>) -> Result<Self, Self::Error> {
        match p {
            Pattern::Symb(symbol, args) => Ok(TopPattern { symbol, args }),
            _ => Err(()),
        }
    }
}

impl<S: Display> Display for Pattern<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Symb(s, pats) => fmt_appl(s, pats, f),
            Self::MVar(m) => write!(f, "μ{}", m),
            Self::Joker => write!(f, "_"),
        }
    }
}
