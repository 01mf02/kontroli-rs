use crate::reduce;
use crate::signature::Signature;
use crate::term::{Arg, RTerm, Term};
use std::fmt;

// DB -> type
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

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "typing error")
    }
}

impl std::error::Error for Error {}

fn assert_convertible(sig: &Signature, tm1: RTerm, tm2: RTerm) -> Result<(), Error> {
    if reduce::convertible(sig, tm1, tm2) {
        Ok(())
    } else {
        Err(Error::Unconvertible)
    }
}

impl Term {
    pub fn infer(&self, sig: &Signature, ctx: &mut Context) -> Result<RTerm, Error> {
        debug!("infer type of {}", self);
        use Term::*;
        match self {
            Kind => Err(Error::KindNotTypable),
            Type => Ok(RTerm::new(Kind)),
            Symb(s) => Ok(sig.get(&s).unwrap().typ.clone()),
            BVar(x) => Ok(ctx.get_type(*x).unwrap()),
            Appl(f, args) => {
                args.iter()
                    .try_fold(f.infer(sig, ctx)?, |ty, arg| match &*ty.whnf(sig) {
                        Prod(Arg { ty: Some(a), .. }, b) => {
                            arg.check(sig, ctx, a.clone())?;
                            Ok(b.clone().subst(&arg))
                        }
                        _ => Err(Error::ProductExpected),
                    })
            }
            Abst(Arg { id, ty: Some(ty) }, tm) => {
                let tm_ty = ctx.bind_of_type(sig, ty.clone(), |ctx| tm.infer(sig, ctx))?;
                match &*tm_ty {
                    Kind => Err(Error::UnexpectedKind),
                    _ => Ok(RTerm::new(Prod(
                        Arg {
                            id: id.clone(),
                            ty: Some(ty.clone()),
                        },
                        tm_ty,
                    ))),
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

    pub fn check(&self, sig: &Signature, ctx: &mut Context, ty_exp: RTerm) -> Result<(), Error> {
        debug!("check {} is of type {} when {}", self, ty_exp, ctx);
        use Term::*;
        match self {
            Abst(arg, tm) => match &*ty_exp.whnf(sig) {
                Prod(Arg { ty: Some(ty_a), .. }, ty_b) => {
                    match arg.ty.clone() {
                        None => Ok(()),
                        Some(ty_a_exp) => {
                            let _ = ty_a_exp.infer(sig, ctx)?;
                            assert_convertible(sig, ty_a_exp, ty_a.clone())
                        }
                    }?;
                    ctx.bind(ty_a.clone(), |ctx| tm.check(sig, ctx, ty_b.clone()))
                }
                _ => Err(Error::ProductExpected),
            },
            _ => {
                let ty_inf = self.infer(sig, ctx)?;
                debug!("checking convertibility: {} ~ {}", ty_inf, ty_exp);
                assert_convertible(sig, ty_inf, ty_exp)
            }
        }
    }
}
