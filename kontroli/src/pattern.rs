//! Rewrite patterns.

use crate::application::format as fmt_appl;
use crate::scope::Symbol;
use crate::{BTerm, Term};
use alloc::vec::Vec;
use core::fmt::{self, Display};
use core::{borrow::Borrow, convert::TryFrom};

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

impl<S> Pattern<S> {
    pub fn try_map<S2, E>(self, f: &impl Fn(S) -> Result<S2, E>) -> Result<Pattern<S2>, E> {
        match self {
            Self::Symb(s, args) => {
                let args = args.into_iter().map(|p| p.try_map(f));
                Ok(Pattern::Symb(f(s)?, args.collect::<Result<_, _>>()?))
            }
            Self::MVar(m) => Ok(Pattern::MVar(m)),
            Self::Joker => Ok(Pattern::Joker),
        }
    }
}

impl<S: Borrow<str>, V> TryFrom<BTerm<Symbol<S>, V>> for Pattern<Symbol<S>> {
    type Error = ();

    fn try_from(tm: BTerm<Symbol<S>, V>) -> Result<Self, Self::Error> {
        use Term::*;
        match tm.get() {
            Symb(s) if s.name.borrow() == "_" && s.path.is_empty() => Ok(Pattern::Joker),
            Symb(s) => Ok(Self::Symb(s, Vec::new())),
            BVar(v) => Ok(Self::MVar(v)),
            Appl(head, args2) => match Self::try_from(head)? {
                Self::Symb(s, mut args) => {
                    let args2 = args2.into_iter().map(Self::try_from);
                    args.append(&mut args2.collect::<Result<_, _>>()?);
                    Ok(Self::Symb(s, args))
                }
                _ => Err(()),
            },
            _ => Err(()),
        }
    }
}

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
