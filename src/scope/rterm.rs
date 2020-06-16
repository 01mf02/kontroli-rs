//! Pointers to terms.

use super::Term;
use crate::parse::term::GArg;
use alloc::{boxed::Box, string::String};

/// Pointer to a term.
pub type RTerm<'s> = Box<Term<'s>>;

/// Argument of a binder.
pub type Arg<'s> = GArg<String, Option<RTerm<'s>>>;
