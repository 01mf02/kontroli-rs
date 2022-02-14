//! Terms for the lambda-Pi calculus.

use crate::app::format as fmt_appl;
use crate::Arg;
use alloc::vec::Vec;
use core::fmt::{self, Display};

/// De Bruijn variable.
pub type DeBruijn = usize;

/// Term for the lambda-Pi calculus.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term<Sym, Tm> {
    Kind,
    Type,
    Symb(Sym),
    BVar(DeBruijn),
    Comb(Tm),
}

/// Combinator term.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TermC<Id, Tm> {
    Appl(Tm, Vec<Tm>),
    Prod(Arg<Id, Tm>, Tm),
    Abst(Arg<Id, Option<Tm>>, Tm),
}

impl<A, C: core::ops::Deref<Target = TermC<V, Self>>, V> Term<A, C> {
    pub fn get_prod(&self) -> Option<(&Arg<V, Self>, &Self)> {
        if let Term::Comb(comb) = self {
            if let TermC::Prod(arg, tm) = &**comb {
                return Some((arg, tm));
            }
        }
        None
    }

    pub fn get_abst(&self) -> Option<(&Arg<V, Option<Self>>, &Self)> {
        if let Term::Comb(comb) = self {
            if let TermC::Abst(arg, tm) = &**comb {
                return Some((arg, tm));
            }
        }
        None
    }
}

impl<C, T> Term<C, T> {
    pub fn try_map<FC, FT, C2, T2, E>(self, fc: FC, ft: FT) -> Result<Term<C2, T2>, E>
    where
        FC: Fn(C) -> Result<C2, E>,
        FT: Fn(T) -> Result<T2, E>,
    {
        match self {
            Self::Kind => Ok(Term::Kind),
            Self::Type => Ok(Term::Type),
            Self::Symb(s) => Ok(Term::Symb(fc(s)?)),
            Self::BVar(b) => Ok(Term::BVar(b)),
            Self::Comb(c) => Ok(Term::Comb(ft(c)?)),
        }
    }
}

impl<V, T> TermC<V, T> {
    pub fn try_map<FV, FT, V2, T2, E>(self, fv: FV, ft: FT) -> Result<TermC<V2, T2>, E>
    where
        FV: Fn(V) -> V2,
        FT: Fn(T) -> Result<T2, E>,
    {
        match self {
            Self::Appl(tm, args) => Ok(TermC::Appl(
                ft(tm)?,
                args.into_iter().map(ft).collect::<Result<_, _>>()?,
            )),
            Self::Prod(arg, tm) => {
                let tm = ft(tm)?;
                Ok(TermC::Prod(arg.map_id(fv).try_map_type(ft)?, tm))
            }
            Self::Abst(arg, tm) => {
                let tm = ft(tm)?;
                let arg = arg.map_id(fv).try_map_type(|ty| ty.map(ft).transpose())?;
                Ok(TermC::Abst(arg, tm))
            }
        }
    }
}

impl<A: Display, C: Display> Display for Term<A, C> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Kind => write!(f, "Kind"),
            Self::Type => write!(f, "Type"),
            Self::Symb(s) => write!(f, "{}", s),
            Self::BVar(x) => write!(f, "β{}", x),
            Self::Comb(c) => c.fmt(f),
        }
    }
}

impl<V: Display, Tm: Display> Display for TermC<V, Tm> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Appl(head, tail) => fmt_appl(head, tail, f),
            Self::Prod(arg, tm) => write!(f, "(Π {} : {}. {})", arg.id, arg.ty, tm),
            Self::Abst(Arg { id, ty: None }, tm) => write!(f, "(λ {}. {})", id, tm),
            Self::Abst(Arg { id, ty: Some(ty) }, tm) => write!(f, "(λ {} : {}. {})", id, ty, tm),
        }
    }
}
