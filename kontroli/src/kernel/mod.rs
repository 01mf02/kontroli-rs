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

use crate::{Arg, Symbol};
use alloc::string::String;

/// Rewrite pattern.
pub type Pattern<'s> = crate::Pattern<Symbol<'s>>;

/// Pattern at the left-hand side of a rewrite rule.
pub type TopPattern<'s> = crate::pattern::TopPattern<Symbol<'s>>;

/// Rewrite rules with strings as bound variable identifiers,
/// a top pattern (symbol application) as left-hand side, and
/// a shared term as right-hand side.
pub type Rule<'s> = crate::Rule<Arg<String, Option<Term<'s>>>, TopPattern<'s>, Term<'s>>;

/// The way we introduce a new name.
pub type Intro<'s> = crate::Intro<Term<'s>>;

pub type GCtx<'s> = crate::GCtx<Symbol<'s>, Pattern<'s>, Term<'s>>;

pub type Term<'s> = crate::Term<Symbol<'s>, RTerm<'s>>;
pub type TermC<'s> = crate::term::TermC<Rc<String>, Term<'s>>;

pub type Typing<'s> = crate::Typing<Term<'s>, Option<(Term<'s>, crate::typing::Check)>>;
