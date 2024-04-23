//! Terms for the lambda-Pi calculus.

use crate::Symbol;
use alloc::{rc::Rc, string::String};
use core::fmt::{self, Display};

pub use crate::lterm::{DeBruijn, LComb, LTerm};
pub use crate::Comb;

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

pub type SComb<'s, 't> = Comb<&'t String, STerm<'s, 't>>;

impl<'s, 't> STerm<'s, 't> {
    pub fn get_comb(self) -> Option<SComb<'s, 't>> {
        if let Self::SComb(comb) = self {
            return Some(Rc::try_unwrap(comb).unwrap_or_else(|rc| (*rc).clone()));
        } else if let Self::LComb(comb) = self {
            return Some(SComb::from(comb));
        }
        None
    }

    pub fn get_abst(&self) -> Option<(&'t String, Option<Self>, Self)> {
        if let Self::SComb(comb) = self {
            if let Comb::Abst(id, ty, tm) = &**comb {
                return Some((id, ty.as_ref().cloned(), tm.clone()));
            }
        } else if let Self::LComb(Comb::Abst(id, ty, tm)) = self {
            return Some((id, ty.as_ref().map(STerm::from), STerm::from(tm)));
        }
        None
    }

    pub fn get_prod(self) -> Option<(&'t String, Self, Self)> {
        if let Some(Comb::Prod(id, ty, tm)) = self.get_comb() {
            return Some((id, ty, tm));
        }
        None
    }

    /// Apply some terms to the term.
    pub fn apply(mut self, args: impl IntoIterator<Item = Self>) -> Self {
        if let Self::SComb(ref mut c) = self {
            if let Some(Comb::Appl(_, args1)) = Rc::get_mut(c) {
                args1.extend(args);
                return self;
            }
        }
        Self::SComb(Rc::new(Comb::Appl(self, args.into_iter().collect())))
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
            STerm::LComb(c) => Ok(Self::Comb((*c).clone().into())),
            STerm::SComb(c) => Ok(Self::Comb(LComb::try_from(&**c)?.into())),
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
