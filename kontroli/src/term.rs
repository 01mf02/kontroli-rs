//! Terms for the lambda-Pi calculus.

use crate::application::format as fmt_appl;
use crate::Arg;
use alloc::vec::Vec;
use core::fmt::{self, Display};

/// De Bruijn variable.
pub type DeBruijn = usize;

/// Term for the lambda-Pi calculus.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term<Sym, Id, Tm> {
    Kind,
    Type,
    Symb(Sym),
    BVar(DeBruijn),
    Appl(Tm, Vec<Tm>),
    Prod(Arg<Id, Tm>, Tm),
    Abst(Arg<Id, Option<Tm>>, Tm),
}

impl<Sym, Id, Tm> Term<Sym, Id, Tm> {
    pub fn map<FId, FTm, Id2, Tm2>(self, fid: FId, ftm: &FTm) -> Term<Sym, Id2, Tm2>
    where
        FId: Fn(Id) -> Id2,
        FTm: Fn(Tm) -> Tm2,
    {
        match self {
            Self::Kind => Term::Kind,
            Self::Type => Term::Type,
            Self::Symb(s) => Term::Symb(s),
            Self::BVar(b) => Term::BVar(b),
            Self::Appl(tm, args) => Term::Appl(ftm(tm), args.into_iter().map(ftm).collect()),
            Self::Prod(arg, tm) => Term::Prod(arg.map_id(fid).map_ty(ftm), ftm(tm)),
            Self::Abst(arg, tm) => Term::Abst(arg.map_id(fid).map_ty(|ty| ty.map(ftm)), ftm(tm)),
        }
    }
}

impl<Sym, Id, Tm> Default for Term<Sym, Id, Tm> {
    fn default() -> Self {
        Self::Type
    }
}

impl<Sym: Display, Id: Display, Tm: Display> Display for Term<Sym, Id, Tm> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Kind => write!(f, "Kind"),
            Self::Type => write!(f, "Type"),
            Self::Symb(s) => write!(f, "{}", s),
            Self::BVar(x) => write!(f, "β{}", x),
            Self::Appl(head, tail) => fmt_appl(head, tail, f),
            Self::Prod(arg, tm) => write!(f, "(Π {} : {}. {})", arg.id, arg.ty, tm),
            Self::Abst(Arg { id, ty: None }, tm) => write!(f, "(λ {}. {})", id, tm),
            Self::Abst(Arg { id, ty: Some(ty) }, tm) => write!(f, "(λ {} : {}. {})", id, ty, tm),
        }
    }
}
