//! Terms for the lambda-Pi calculus.

use crate::{Comb, Pattern, Symbol};
use alloc::{boxed::Box, string::String};
use core::fmt::{self, Display};

/// De Bruijn variable.
pub type DeBruijn = usize;

/// Long term for the lambda-Pi calculus.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LTerm<'s> {
    Type,
    Const(Symbol<'s>),
    Var(DeBruijn),
    // you can replace `Box` by `alloc::sync::Arc`
    // to make `LTerm` capable of sharing subterms
    // however, this, on its own, will not actually share subterms yet
    Comb(Box<LComb<'s>>),
}

pub type LComb<'s> = Comb<String, LTerm<'s>>;

impl<'s> TryFrom<Pattern<Symbol<'s>>> for LTerm<'s> {
    type Error = ();

    fn try_from(p: Pattern<Symbol<'s>>) -> Result<Self, Self::Error> {
        use Pattern::*;
        match p {
            Symb(s, args) => {
                let args: Result<_, _> = args.into_iter().map(Self::try_from).collect();
                Ok(LTerm::Comb(Comb::Appl(LTerm::Const(s), args?).into()))
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
