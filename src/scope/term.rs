//! Terms for the lambda-Pi calculus.

use super::Symbol;
use crate::fmt::application as fmt_appl;
use crate::pre::term::GArg;
use alloc::{boxed::Box, string::String, vec::Vec};
use core::fmt;

/// Pointer to a term.
pub type RTerm<'s> = Box<Term<'s>>;

/// Argument of a binder.
pub type Arg<'s> = GArg<String, Option<RTerm<'s>>>;

/// De Bruijn variable.
pub type DeBruijn = usize;

/// Term for the lambda-Pi calculus.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term<'s> {
    Kind,
    Type,
    Symb(Symbol<'s>),
    BVar(DeBruijn),
    Appl(RTerm<'s>, Vec<Term<'s>>),
    Abst(Arg<'s>, RTerm<'s>),
    Prod(Arg<'s>, RTerm<'s>),
}

impl<'s> fmt::Display for Arg<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.id)?;
        if let Some(ty) = self.ty.as_ref() {
            write!(f, " : {}", ty)?;
        }
        Ok(())
    }
}

impl<'s> fmt::Display for Term<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Kind => write!(f, "Kind"),
            Self::Type => write!(f, "Type"),
            Self::Symb(s) => write!(f, "{}", s),
            Self::BVar(x) => write!(f, "β{}", x),
            Self::Appl(head, tail) => fmt_appl(head, tail, f),
            Self::Abst(arg, tm) => write!(f, "(λ {}. {})", arg, tm),
            Self::Prod(arg, tm) => write!(f, "(Π {}. {})", arg, tm),
        }
    }
}

impl<'s> Arg<'s> {
    pub fn new(id: String, ty: Option<RTerm<'s>>) -> Self {
        Self { id, ty }
    }
}
