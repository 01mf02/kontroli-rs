//! Type checking and type inference for terms.

use super::{GCtx, RTerm, Term, TermC};
use crate::error::TypingError as Error;
use crate::Arg;
use core::fmt;

type Result<T> = core::result::Result<T, Error>;

/// Map from de Bruijn indices to associated types.
type LCtx<'s> = crate::Stack<Term<'s>>;

impl<'s> LCtx<'s> {
    fn get_type(&self, n: usize) -> Option<Term<'s>> {
        Some(self.get(n)?.clone() << (n + 1))
    }

    fn bind<A, F>(&mut self, arg: Term<'s>, f: F) -> Result<A>
    where
        F: FnOnce(&mut LCtx<'s>) -> Result<A>,
    {
        self.try_with_pushed(arg, f)
    }

    fn bind_of_type<A, F>(&mut self, gc: &GCtx<'s>, arg: Term<'s>, f: F) -> Result<A>
    where
        F: FnOnce(&mut LCtx<'s>) -> Result<A>,
    {
        match arg.clone().infer(gc, self)? {
            Term::Type => self.bind(arg, f),
            _ => Err(Error::BindNoType),
        }
    }
}

impl<'s> fmt::Display for LCtx<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        for (i, x) in self.iter().enumerate() {
            write!(f, "{} : {}, ", Term::BVar(i), x.clone() << (i + 1))?;
        }
        write!(f, "]")
    }
}

impl<'s> Term<'s> {
    /// Infer the type of a term using supplied types of bound variables.
    pub fn infer(&self, gc: &GCtx<'s>, lc: &mut LCtx<'s>) -> Result<Term<'s>> {
        debug!("infer type of {}", self);
        use crate::Term::*;
        match self {
            Kind => Err(Error::KindNotTypable),
            Type => Ok(Kind),
            Symb(s) => Ok(gc.types.get(s).ok_or(Error::TypeNotFound)?.clone()),
            BVar(x) => Ok(lc.get_type(*x).ok_or(Error::TypeNotFound)?),
            Comb(c) => c.infer(gc, lc),
        }
    }

    /// Check whether a term is of the given type, using supplied types of bound variables.
    pub fn check(&self, gc: &GCtx<'s>, lc: &mut LCtx<'s>, ty_exp: Term<'s>) -> Result<bool> {
        debug!("check {} is of type {} when {}", self, ty_exp, lc);
        if let Some((arg, tm)) = self.get_abst() {
            let whnf = ty_exp.whnf(gc);
            let (Arg { ty: ty_a, .. }, ty_b) = whnf.get_prod().ok_or(Error::ProductExpected)?;
            let a_ok = match &arg.ty {
                None => true,
                Some(ty) => {
                    let _ = ty.infer(gc, lc)?;
                    Term::convertible(ty.clone(), ty_a.clone(), gc)
                }
            };
            Ok(a_ok && lc.bind(ty_a.clone(), |lc| tm.check(gc, lc, ty_b.clone()))?)
        } else {
            let ty_inf = self.infer(gc, lc)?;
            debug!("checking convertibility: {} ~ {}", ty_inf, ty_exp);
            Ok(Term::convertible(ty_inf, ty_exp, gc))
        }
    }
}

impl<'s> TermC<'s> {
    fn infer(&self, gc: &GCtx<'s>, lc: &mut LCtx<'s>) -> Result<Term<'s>> {
        use crate::term::{Term::*, TermC::*};
        match self {
            Appl(tm, args) => {
                let tm_ty = tm.infer(gc, lc)?;
                args.iter().try_fold(tm_ty, |ty, arg| {
                    let whnf = ty.whnf(gc);
                    let (Arg { ty: a, .. }, b) = whnf.get_prod().ok_or(Error::ProductExpected)?;
                    if arg.check(gc, lc, a.clone())? {
                        Ok(b.clone().subst(arg))
                    } else {
                        Err(Error::Unconvertible)
                    }
                })
            }
            Abst(Arg { id, ty: Some(ty) }, tm) => {
                let tm_ty = lc.bind_of_type(gc, ty.clone(), |lc| tm.infer(gc, lc))?;
                if tm_ty == Kind {
                    Err(Error::UnexpectedKind)
                } else {
                    let id = id.clone();
                    let ty = ty.clone();
                    Ok(Comb(RTerm::new(Prod(Arg { id, ty }, tm_ty))))
                }
            }
            Prod(Arg { ty, .. }, tm) => {
                let tm_ty = lc.bind_of_type(gc, ty.clone(), |lc| tm.infer(gc, lc))?;
                match tm_ty {
                    Kind | Type => Ok(tm_ty),
                    _ => Err(Error::SortExpected),
                }
            }
            Abst(Arg { ty: None, .. }, _) => Err(Error::DomainFreeAbstraction),
        }
    }
}
