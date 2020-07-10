mod convertible;
mod matching;
#[path = "../scope/pattern.rs"]
pub mod pattern;
mod reduce;
pub mod rterm;
mod share;
pub mod state;
mod subst;
#[path = "../scope/term.rs"]
pub mod term;
mod typing;

// We have a hole here which can be instantiated with any
// pointer type that implements `ptr_eq`, such as `Arc` or `Rc`.
// This trick might become unnecessary once GATs are available in Rust.
use super::Rc;

pub use pattern::Pattern;
pub use rterm::RTerm;
pub use term::Term;
pub use typing::Typing;

use crate::scope::Symbol;
use alloc::string::String;

/// Rewrite rules with strings as bound variable identifiers,
/// a top pattern (symbol application) as left-hand side, and
/// a shared term as right-hand side.
pub type Rule<'s> = crate::Rule<String, pattern::TopPattern<'s>, RTerm<'s>>;

/// The way we introduce a new name.
pub type Intro<'s> = crate::Intro<RTerm<'s>, RTerm<'s>>;

pub type Signature<'s> = crate::Signature<Symbol<'s>, Pattern<'s>, RTerm<'s>>;
