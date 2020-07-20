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

use crate::scope::pattern::{Pattern, TopPattern};
use crate::scope::Symbol;
use alloc::string::String;

/// Rewrite rules with strings as bound variable identifiers,
/// a top pattern (symbol application) as left-hand side, and
/// a shared term as right-hand side.
pub type Rule<'s> = crate::Rule<String, TopPattern<'s>, RTerm<'s>>;

/// The way we introduce a new name.
pub type Intro<'s> = crate::Intro<RTerm<'s>, RTerm<'s>>;

pub type Signature<'s> = crate::Signature<Symbol<'s>, Pattern<'s>, RTerm<'s>>;

pub type Term<'s> = crate::Term<Symbol<'s>, Rc<String>, RTerm<'s>>;
