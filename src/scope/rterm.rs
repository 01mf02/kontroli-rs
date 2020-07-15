//! Pointers to terms.

use super::Term;
use alloc::{boxed::Box, string::String};

/// Pointer to a term.
pub type RTerm<'s> = Box<Term<'s>>;

/// Argument of a binder.
pub type Arg<'s> = crate::Arg<String, RTerm<'s>>;

pub type OptArg<'s> = crate::Arg<String, Option<RTerm<'s>>>;
