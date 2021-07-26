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

impl<C, V, T> Term<C, V, T> {
    pub fn try_map<FC, FV, FT, C2, V2, T2, E>(
        self,
        fc: FC,
        fv: FV,
        ft: FT,
    ) -> Result<Term<C2, V2, T2>, E>
    where
        FC: Fn(C) -> Result<C2, E>,
        FV: Fn(V) -> V2,
        FT: Fn(T) -> Result<T2, E>,
    {
        match self {
            Self::Kind => Ok(Term::Kind),
            Self::Type => Ok(Term::Type),
            Self::Symb(s) => Ok(Term::Symb(fc(s)?)),
            Self::BVar(b) => Ok(Term::BVar(b)),
            Self::Appl(tm, args) => Ok(Term::Appl(
                ft(tm)?,
                args.into_iter().map(ft).collect::<Result<_, _>>()?,
            )),
            Self::Prod(arg, tm) => {
                let tm = ft(tm)?;
                Ok(Term::Prod(arg.map_id(fv).try_map_type(ft)?, tm))
            }
            Self::Abst(arg, tm) => {
                let tm = ft(tm)?;
                let arg = arg.map_id(fv).try_map_type(|ty| ty.map(ft).transpose())?;
                Ok(Term::Abst(arg, tm))
            }
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
