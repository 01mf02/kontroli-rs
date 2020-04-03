//! Type checking and type inference for terms.

use crate::term::{Arg, RTerm, Term};
use crate::Signature;
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
        match &*arg.clone().infern(sig, self)? {
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
    TypeAndTermEmpty,
}

impl Arg {
    /// Check whether the bound variable's type (if present)
    /// has a proper type and is convertible with the given type.
    fn checkn(&self, sig: &Signature, ctx: &mut Context, ty_exp: &RTerm) -> Result<bool, Error> {
        match self.ty.clone() {
            None => Ok(true),
            Some(ty) => {
                let _ = ty.infern(sig, ctx)?;
                Ok(RTerm::convertible(ty, ty_exp.clone(), sig))
            }
        }
    }
}

impl Term {
    /// Infer the type of a closed term.
    pub fn infer(&self, sig: &Signature) -> Result<RTerm, Error> {
        self.infern(sig, &mut Context::new())
    }

    /// Check whether a closed term is of a given type.
    pub fn check(&self, sig: &Signature, ty_exp: RTerm) -> Result<bool, Error> {
        self.checkn(sig, &mut Context::new(), ty_exp)
    }

    /// Infer the type of an open term using supplied types of bound variables.
    pub fn infern(&self, sig: &Signature, ctx: &mut Context) -> Result<RTerm, Error> {
        debug!("infer type of {}", self);
        use Term::*;
        match self {
            Kind => Err(Error::KindNotTypable),
            Type => Ok(RTerm::new(Kind)),
            Symb(s) => Ok(sig.types.get(&s).unwrap().clone()),
            BVar(x) => Ok(ctx.get_type(*x).unwrap()),
            Appl(f, args) => {
                args.iter()
                    .try_fold(f.infern(sig, ctx)?, |ty, arg| match &*ty.whnf(sig) {
                        Prod(Arg { ty: Some(a), .. }, b) => {
                            if arg.checkn(sig, ctx, a.clone())? {
                                Ok(b.clone().subst(&arg))
                            } else {
                                Err(Error::Unconvertible)
                            }
                        }
                        _ => Err(Error::ProductExpected),
                    })
            }
            Abst(Arg { id, ty: Some(ty) }, tm) => {
                let tm_ty = ctx.bind_of_type(sig, ty.clone(), |ctx| tm.infern(sig, ctx))?;
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
                let tm_ty = ctx.bind_of_type(sig, ty.clone(), |ctx| tm.infern(sig, ctx))?;
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

    /// Check whether an open term is of the given type,
    /// using supplied types of bound variables.
    pub fn checkn(&self, sig: &Signature, ctx: &mut Context, ty_exp: RTerm) -> Result<bool, Error> {
        debug!("check {} is of type {} when {}", self, ty_exp, ctx);
        use Term::*;
        match self {
            Abst(arg, tm) => match &*ty_exp.whnf(sig) {
                Prod(Arg { ty: Some(ty_a), .. }, ty_b) => Ok(arg.checkn(sig, ctx, ty_a)?
                    && ctx.bind(ty_a.clone(), |ctx| tm.checkn(sig, ctx, ty_b.clone()))?),
                _ => Err(Error::ProductExpected),
            },
            _ => {
                let ty_inf = self.infern(sig, ctx)?;
                debug!("checking convertibility: {} ~ {}", ty_inf, ty_exp);
                Ok(RTerm::convertible(ty_inf, ty_exp, sig))
            }
        }
    }
}
