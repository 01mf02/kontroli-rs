//! Type checking and type inference for terms.

use super::sterm::{SComb, STerm};
use super::GCtx;
use crate::error::TypingError as Error;
use crate::Arg;
use alloc::vec::Vec;
use core::fmt;

type Result<T> = core::result::Result<T, Error>;

/// Map from de Bruijn indices to associated types.
#[derive(Default)]
pub struct LCtx<'s, 't>(Vec<STerm<'s, 't>>);

impl<'s, 't> LCtx<'s, 't> {
    fn get_type(&self, n: usize) -> Option<STerm<'s, 't>> {
        Some(self.0.iter().rev().nth(n)?.clone().shift(n + 1))
    }

    fn bind<A, F>(&mut self, arg: STerm<'s, 't>, f: F) -> Result<A>
    where
        F: FnOnce(&mut LCtx<'s, 't>) -> Result<A>,
    {
        self.0.push(arg);
        let y = f(self)?;
        self.0.pop();
        Ok(y)
    }

    fn bind_of_type<A, F>(&mut self, gc: &'t GCtx<'s>, arg: &STerm<'s, 't>, f: F) -> Result<A>
    where
        F: FnOnce(&mut LCtx<'s, 't>) -> Result<A>,
    {
        match arg.infer(gc, self)? {
            STerm::Type => self.bind(arg.clone(), f),
            _ => Err(Error::BindNoType),
        }
    }
}

impl<'s, 't> fmt::Display for LCtx<'s, 't> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        for (i, x) in self.0.iter().rev().enumerate() {
            write!(f, "{} : {}, ", STerm::Var(i), x.clone().shift(i + 1))?;
        }
        write!(f, "]")
    }
}

impl<'s, 't> FromIterator<STerm<'s, 't>> for LCtx<'s, 't> {
    fn from_iter<I: IntoIterator<Item = STerm<'s, 't>>>(iter: I) -> Self {
        Self(Vec::from_iter(iter))
    }
}

impl<'s, 't> STerm<'s, 't> {
    /// Infer the type of a term using supplied types of bound variables.
    pub fn infer(&self, gc: &'t GCtx<'s>, lc: &mut LCtx<'s, 't>) -> Result<STerm<'s, 't>> {
        debug!("infer type of {}", self);
        match self {
            Self::Kind => Err(Error::KindNotTypable),
            Self::Type => Ok(Self::Kind),
            Self::Const(c) => Ok((&**gc.get_type(c).ok_or(Error::TypeNotFound)?).into()),
            Self::Var(v) => Ok(lc.get_type(*v).ok_or(Error::TypeNotFound)?),
            Self::SComb(c) => c.infer(gc, lc),
            Self::LComb(c) => SComb::from(*c).infer(gc, lc),
        }
    }

    /// Check whether a term is of the given type, using supplied types of bound variables.
    pub fn check(&self, gc: &'t GCtx<'s>, lc: &mut LCtx<'s, 't>, ty: Self) -> Result<bool> {
        debug!("check {} is of type {} when {}", self, ty, lc);
        if let Some((arg, tm)) = self.get_abst() {
            let whnf = ty.whnf(gc);
            let (Arg { ty: ty_a, .. }, ty_b) = whnf.get_prod().ok_or(Error::ProductExpected)?;
            if let Some(arg_ty) = arg.ty {
                let _ = arg_ty.infer(gc, lc)?;
                if !Self::convertible(arg_ty, ty_a.clone(), gc) {
                    return Ok(false);
                }
            };
            Ok(lc.bind(ty_a, |lc| tm.check(gc, lc, ty_b))?)
        } else {
            let ty_inf = self.infer(gc, lc)?;
            debug!("checking convertibility: {} ~ {}", ty_inf, ty);
            Ok(Self::convertible(ty_inf, ty, gc))
        }
    }
}

impl<'s, 't> SComb<'s, 't> {
    fn infer(&self, gc: &'t GCtx<'s>, lc: &mut LCtx<'s, 't>) -> Result<STerm<'s, 't>> {
        use STerm::*;
        match self {
            Self::Appl(tm, args) => {
                let tm_ty = tm.infer(gc, lc)?;
                args.iter().try_fold(tm_ty, |ty, arg| {
                    let whnf = ty.whnf(gc);
                    let (Arg { ty: a, .. }, b) = whnf.get_prod().ok_or(Error::ProductExpected)?;
                    arg.check(gc, lc, a)?
                        .then(|| b.subst(arg))
                        .ok_or(Error::Unconvertible)
                })
            }
            Self::Abst(Arg { id, ty: Some(ty) }, tm) => {
                let tm_ty = lc.bind_of_type(gc, ty, |lc| tm.infer(gc, lc))?;
                if tm_ty == Kind {
                    Err(Error::UnexpectedKind)
                } else {
                    let ty = ty.clone();
                    Ok(SComb(Self::Prod(Arg { id, ty }, tm_ty).into()))
                }
            }
            Self::Prod(Arg { ty, .. }, tm) => Some(lc.bind_of_type(gc, ty, |lc| tm.infer(gc, lc))?)
                .filter(|tt| matches!(tt, Kind | Type))
                .ok_or(Error::SortExpected),
            Self::Abst(Arg { ty: None, .. }, _) => Err(Error::DomainFreeAbstraction),
        }
    }
}
