mod convertible;
mod matching;
mod reduce;
pub mod rterm;
mod share;
pub mod state;
mod subst;
mod typing;

// We have a hole here which can be instantiated with any
// pointer type that implements `ptr_eq`, such as `Arc` or `Rc`.
// This trick might become unnecessary once GATs are available in Rust.
use super::Rc;

pub use rterm::RTerm;
pub use typing::Typing;

use crate::{Arg, Symbol};
use alloc::string::String;

/// Rewrite pattern.
pub type Pattern<'s> = crate::Pattern<Symbol<'s>>;

/// Pattern at the left-hand side of a rewrite rule.
pub type TopPattern<'s> = crate::pattern::TopPattern<Symbol<'s>>;

/// Rewrite rules with strings as bound variable identifiers,
/// a top pattern (symbol application) as left-hand side, and
/// a shared term as right-hand side.
pub type Rule<'s> = crate::Rule<Arg<String, Option<RTerm<'s>>>, TopPattern<'s>, RTerm<'s>>;

/// The way we introduce a new name.
pub type Intro<'s> = crate::Intro<RTerm<'s>>;

pub type GCtx<'s> = crate::GCtx<Symbol<'s>, Pattern<'s>, RTerm<'s>>;

pub type Term<'s> = crate::Term<Symbol<'s>, Rc<String>, RTerm<'s>>;

impl<'s> core::convert::TryFrom<TopPattern<'s>> for RTerm<'s> {
    type Error = ();
    fn try_from(p: TopPattern<'s>) -> Result<Self, Self::Error> {
        Ok(Self::new(Term::try_from(Pattern::from(p))?))
    }
}
