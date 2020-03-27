//! Type checking and type inference for terms.

use crate::reduce::convertible;
use crate::signature::Signature;
use crate::term::{Arg, RTerm, Term};
use core::fmt;

/// Map from de Bruijn indices to associated types.
pub type Context = crate::stack::Stack<RTerm>;

impl Context {
    fn get_type(&self, n: usize) -> Option<RTerm> {
        Some(self.get(n)?.clone() << (n + 1))
    }

    fn bind<A, F>(&mut self, arg: RTerm, f: F) -> Result<A, Error>
    where
        F: FnOnce(&mut Context) -> Result<A, Error>,
    {
        self.with_pushed(arg, f)
    }

    fn bind_of_type<A, F>(&mut self, sig: &Signature, arg: RTerm, f: F) -> Result<A, Error>
    where
        F: FnOnce(&mut Context) -> Result<A, Error>,
    {
        match &*arg.clone().infer(sig, self)? {
            Term::Type => self.bind(arg, f),
            _ => Err(Error::BindNoType),
        }
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        for (i, x) in self.iter().enumerate() {
            write!(f, "{} : {}, ", Term::BVar(i), x.clone() << (i + 1))?;
        }
        write!(f, "]")
    }
}

#[derive(Debug)]
pub enum Error {
    ProductExpected,
    SortExpected,
    BindNoType,
    Unconvertible,
    KindNotTypable,
    UnexpectedKind,
    DomainFreeAbstraction,
}

impl Arg {
    /// Check whether the bound variable's type (if present)
    /// has a proper type and is convertible with the given type.
    fn check(&self, sig: &Signature, ctx: &mut Context, ty_exp: &RTerm) -> Result<bool, Error> {
        match self.ty.clone() {
            None => Ok(true),
            Some(ty) => {
                let _ = ty.infer(sig, ctx)?;
                Ok(convertible(sig, ty, ty_exp.clone()))
            }
        }
    }
}

impl Term {
    /// Infer the type of a closed term.
    pub fn infer_closed(&self, sig: &Signature) -> Result<RTerm, Error> {
        self.infer(sig, &mut Context::new())
    }

    /// Check whether a closed term is of a given type.
    pub fn check_closed(&self, sig: &Signature, ty_exp: RTerm) -> Result<bool, Error> {
        self.check(sig, &mut Context::new(), ty_exp)
    }

    pub fn infer(&self, sig: &Signature, ctx: &mut Context) -> Result<RTerm, Error> {
        debug!("infer type of {}", self);
        use Term::*;
        match self {
            Kind => Err(Error::KindNotTypable),
            Type => Ok(RTerm::new(Kind)),
            Symb(s) => Ok(sig.types.get(&s).unwrap().clone()),
            BVar(x) => Ok(ctx.get_type(*x).unwrap()),
            Appl(f, args) => {
                args.iter()
                    .try_fold(f.infer(sig, ctx)?, |ty, arg| match &*ty.whnf(sig) {
                        Prod(Arg { ty: Some(a), .. }, b) => {
                            if arg.check(sig, ctx, a.clone())? {
                                Ok(b.clone().subst(&arg))
                            } else {
                                Err(Error::Unconvertible)
                            }
                        }
                        _ => Err(Error::ProductExpected),
                    })
            }
            Abst(Arg { id, ty: Some(ty) }, tm) => {
                let tm_ty = ctx.bind_of_type(sig, ty.clone(), |ctx| tm.infer(sig, ctx))?;
                match &*tm_ty {
                    Kind => Err(Error::UnexpectedKind),
                    _ => {
                        let id = id.clone();
                        let ty = Some(ty.clone());
                        Ok(RTerm::new(Prod(Arg { id, ty }, tm_ty)))
                    }
                }
            }
            Prod(Arg { ty: Some(ty), .. }, tm) => {
                let tm_ty = ctx.bind_of_type(sig, ty.clone(), |ctx| tm.infer(sig, ctx))?;
                match &*tm_ty {
                    Kind | Type => Ok(tm_ty),
                    _ => Err(Error::SortExpected),
                }
            }
            Abst(Arg { ty: None, .. }, _) | Prod(Arg { ty: None, .. }, _) => {
                Err(Error::DomainFreeAbstraction)
            }
        }
    }

    pub fn check(&self, sig: &Signature, ctx: &mut Context, ty_exp: RTerm) -> Result<bool, Error> {
        debug!("check {} is of type {} when {}", self, ty_exp, ctx);
        use Term::*;
        match self {
            Abst(arg, tm) => match &*ty_exp.whnf(sig) {
                Prod(Arg { ty: Some(ty_a), .. }, ty_b) => Ok(arg.check(sig, ctx, ty_a)?
                    && ctx.bind(ty_a.clone(), |ctx| tm.check(sig, ctx, ty_b.clone()))?),
                _ => Err(Error::ProductExpected),
            },
            _ => {
                let ty_inf = self.infer(sig, ctx)?;
                debug!("checking convertibility: {} ~ {}", ty_inf, ty_exp);
                Ok(convertible(sig, ty_inf, ty_exp))
            }
        }
    }
}
