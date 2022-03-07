//! Terms for the lambda-Pi calculus.

use crate::Symbol;
use alloc::{boxed::Box, rc::Rc};
use core::fmt::{self, Display};

pub use crate::lterm::{Comb, DeBruijn, LComb, LTerm};

/// Short term for the lambda-Pi calculus.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum STerm<'s, 't> {
    Kind,
    Type,
    Const(Symbol<'s>),
    Var(DeBruijn),
    LComb(&'t LComb<'s>),
    SComb(Rc<SComb<'s, 't>>),
}

pub type SComb<'s, 't> = Comb<&'t str, STerm<'s, 't>>;

impl<'s, 't> STerm<'s, 't> {
    pub fn ptr_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Type, Self::Type) | (Self::Kind, Self::Kind) => true,
            (Self::Const(c1), Self::Const(c2)) => c1 == c2,
            (Self::Var(v1), Self::Var(v2)) => v1 == v2,
            (Self::SComb(l), Self::SComb(r)) => Rc::ptr_eq(l, r),
            (Self::LComb(l), Self::LComb(r)) => core::ptr::eq(l, r),
            _ => false,
        }
    }

    pub fn get_comb(self) -> Option<SComb<'s, 't>> {
        if let Self::SComb(comb) = self {
            return Some(Rc::try_unwrap(comb).unwrap_or_else(|rc| (*rc).clone()));
        } else if let Self::LComb(comb) = self {
            return Some(SComb::from(comb));
        }
        None
    }

    pub fn get_abst(&self) -> Option<(&'t str, Option<Self>, Self)> {
        if let Self::SComb(comb) = self {
            if let Comb::Abst(id, ty, tm) = &**comb {
                return Some((id, ty.as_ref().cloned(), tm.clone()));
            }
        } else if let Self::LComb(Comb::Abst(id, ty, tm)) = self {
            return Some((id, ty.as_ref().map(STerm::from), STerm::from(tm)));
        }
        None
    }

    pub fn get_prod(self) -> Option<(&'t str, Self, Self)> {
        if let Some(Comb::Prod(id, ty, tm)) = self.get_comb() {
            return Some((id, ty, tm));
        }
        None
    }

    /// Apply some terms to the term.
    pub fn apply(mut self, args: impl Iterator<Item = Self>) -> Self {
        if let Self::SComb(ref mut c) = self {
            if let Some(Comb::Appl(_, args1)) = Rc::get_mut(c) {
                args1.extend(args);
                return self;
            }
        }
        Self::SComb(Rc::new(Comb::Appl(self, args.collect())))
    }
}

#[derive(Debug)]
pub struct UnexpectedKind;

impl<'s, 't> From<&'t LTerm<'s>> for STerm<'s, 't> {
    fn from(tm: &'t LTerm<'s>) -> Self {
        match tm {
            LTerm::Type => Self::Type,
            LTerm::Const(c) => Self::Const(*c),
            LTerm::Var(v) => Self::Var(*v),
            LTerm::Comb(c) => Self::LComb(&*c),
        }
    }
}

impl<'s, 't> TryFrom<&STerm<'s, 't>> for LTerm<'s> {
    type Error = UnexpectedKind;
    fn try_from(tm: &STerm<'s, 't>) -> Result<LTerm<'s>, Self::Error> {
        match tm {
            STerm::Kind => Err(UnexpectedKind),
            STerm::Type => Ok(Self::Type),
            STerm::Const(c) => Ok(Self::Const(*c)),
            STerm::Var(v) => Ok(Self::Var(*v)),
            STerm::LComb(c) => Ok(Self::Comb(Box::new((*c).clone()))),
            STerm::SComb(c) => Ok(Self::Comb(Box::new((&**c).try_into()?))),
        }
    }
}

impl<'s, 't> From<&'t LComb<'s>> for SComb<'s, 't> {
    fn from(comb: &'t LComb<'s>) -> Self {
        match comb {
            Comb::Appl(head, args) => {
                Self::Appl(head.into(), args.iter().map(STerm::from).collect())
            }
            Comb::Prod(id, ty, tm) => Self::Prod(&*id, STerm::from(ty), tm.into()),
            Comb::Abst(id, ty, tm) => Self::Abst(&*id, ty.as_ref().map(STerm::from), tm.into()),
        }
    }
}

impl<'s, 't> TryFrom<&SComb<'s, 't>> for LComb<'s> {
    type Error = UnexpectedKind;
    fn try_from(comb: &SComb<'s, 't>) -> Result<LComb<'s>, Self::Error> {
        match comb {
            Comb::Appl(head, args) => Ok(Self::Appl(
                head.try_into()?,
                args.iter().map(LTerm::try_from).collect::<Result<_, _>>()?,
            )),
            Comb::Prod(id, ty, tm) => Ok(Self::Prod(
                (*id).into(),
                LTerm::try_from(ty)?,
                LTerm::try_from(tm)?,
            )),
            Comb::Abst(id, ty, tm) => Ok(Self::Abst(
                (*id).into(),
                ty.as_ref().map(LTerm::try_from).transpose()?,
                LTerm::try_from(tm)?,
            )),
        }
    }
}

impl<'s, 't> Display for STerm<'s, 't> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Kind => write!(f, "Kind"),
            Self::Type => write!(f, "Type"),
            Self::Var(v) => write!(f, "β{}", v),
            Self::Const(c) => c.fmt(f),
            Self::LComb(c) => c.fmt(f),
            Self::SComb(c) => c.fmt(f),
        }
    }
}
