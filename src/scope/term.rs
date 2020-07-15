//! Terms for the lambda-Pi calculus.

use super::rterm::{Arg, OptArg, RTerm};
use super::Symbol;
use crate::application::format as fmt_appl;
use alloc::vec::Vec;
use core::fmt::{self, Display};

/// De Bruijn variable.
pub type DeBruijn = usize;

/// Term for the lambda-Pi calculus.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term<'s> {
    Kind,
    Type,
    Symb(Symbol<'s>),
    BVar(DeBruijn),
    Appl(RTerm<'s>, Vec<RTerm<'s>>),
    Abst(OptArg<'s>, RTerm<'s>),
    Prod(Arg<'s>, RTerm<'s>),
}

impl<'s> Default for Term<'s> {
    fn default() -> Self {
        Self::Type
    }
}

impl<'s> Display for Term<'s> {
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

impl<'s> Display for Arg<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} : {}", self.id, self.ty)
    }
}

impl<'s> Display for OptArg<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.ty.as_ref() {
            None => write!(f, "{}", self.id),
            Some(ty) => write!(f, "{} : {}", self.id, ty),
        }
    }
}
