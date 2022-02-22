//! Terms for the lambda-Pi calculus.

use crate::{Arg, Pattern, Symbol};
use alloc::{boxed::Box, string::String, vec::Vec};
use core::fmt::{self, Display};

/// De Bruijn variable.
pub type DeBruijn = usize;

/// Long term for the lambda-Pi calculus.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LTerm<'s> {
    Type,
    Const(Symbol<'s>),
    Var(DeBruijn),
    Comb(Box<LComb<'s>>),
}

pub type LComb<'s> = Comb<String, LTerm<'s>>;

/// Combinator term.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Comb<Id, Tm> {
    Appl(Tm, Vec<Tm>),
    Prod(Arg<Id, Tm>, Tm),
    Abst(Arg<Id, Option<Tm>>, Tm),
}

impl<'s> TryFrom<Pattern<Symbol<'s>>> for LTerm<'s> {
    type Error = ();

    fn try_from(p: Pattern<Symbol<'s>>) -> Result<Self, Self::Error> {
        use Pattern::*;
        match p {
            Symb(s, args) => {
                let args: Result<_, _> = args.into_iter().map(Self::try_from).collect();
                Ok(LTerm::Comb(Box::new(Comb::Appl(LTerm::Const(s), args?))))
            }
            MVar(v) => Ok(LTerm::Var(v)),
            Joker => Err(()),
        }
    }
}

impl<'s> Display for LTerm<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Type => write!(f, "Type"),
            Self::Var(v) => write!(f, "β{}", v),
            Self::Const(c) => c.fmt(f),
            Self::Comb(c) => c.fmt(f),
        }
    }
}

impl<V: Display, Tm: Display> Display for Comb<V, Tm> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Appl(head, tail) => crate::app::format(head, tail, f),
            Self::Prod(arg, tm) => write!(f, "(Π {} : {}. {})", arg.id, arg.ty, tm),
            Self::Abst(Arg { id, ty: None }, tm) => write!(f, "(λ {}. {})", id, tm),
            Self::Abst(Arg { id, ty: Some(ty) }, tm) => write!(f, "(λ {} : {}. {})", id, ty, tm),
        }
    }
}