pub mod convertible;
mod infer_check;
mod reduce;
pub mod sterm;
mod subst;
pub mod typing;

use crate::lterm::LTerm;
use crate::{Arg, Symbol};
use alloc::string::String;

/// Rewrite pattern.
pub type Pattern<'s> = crate::Pattern<Symbol<'s>>;

/// Pattern at the left-hand side of a rewrite rule.
pub type TopPattern<'s> = crate::pattern::TopPattern<Symbol<'s>>;

/// Rewrite rules with strings as bound variable identifiers,
/// a top pattern (symbol application) as left-hand side, and
/// a shared term as right-hand side.
pub type Rule<'s> = crate::Rule<Arg<String, Option<LTerm<'s>>>, TopPattern<'s>, LTerm<'s>>;

/// The way we introduce a new name.
pub type Intro<'s> = crate::Intro<LTerm<'s>>;

pub type GCtx<'s> = crate::GCtx<Symbol<'s>, Pattern<'s>, LTerm<'s>>;
