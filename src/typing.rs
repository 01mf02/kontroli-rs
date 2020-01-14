use super::Term::*;
use super::*;
use crate::signature::Signature;

// DB -> type
type Context = Vec<Term>;

#[derive(Debug)]
pub enum Error {
    ProductExpected,
    SortExpected,
    Unconvertible,
    KindNotTypable,
    UnexpectedKind,
    DomainFreeAbstraction,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "typing error")
    }
}

impl std::error::Error for Error {}

fn bind<F, A>(sig: &Signature, ctx: &mut Context, ty: Term, f: F) -> Result<A, Error>
where
    F: FnOnce(&mut Context) -> Result<A, Error>,
{
    match ty.infer(sig, ctx)? {
        Type => Ok(scope::bind(ctx, Some(ty), |ctx| f(ctx))?),
        _ => Err(Error::Unconvertible),
    }
}

impl Term {
    pub fn infer(&self, sig: &Signature, ctx: &mut Context) -> Result<Term, Error> {
        match self {
            Kind => Err(Error::KindNotTypable),
            Type => Ok(Kind),
            Symb(s) => Ok(sig.get(&s).unwrap().clone()),
            BVar(x) => Ok(ctx.iter().rev().nth(*x).unwrap().clone()),
            Appl(f, args) => {
                args.iter()
                    .try_fold(f.infer(sig, ctx)?, |ty, arg| match ty.whnf(sig) {
                        Prod(Arg { ty: Some(a), .. }, b) => {
                            arg.check(sig, ctx, *a)?;
                            Ok(b.subst(&arg))
                        }
                        _ => Err(Error::ProductExpected),
                    })
            }
            Abst(Arg { id, ty: Some(ty) }, tm) => {
                match bind(sig, ctx, *ty.clone(), |ctx| tm.infer(sig, ctx))? {
                    Kind => Err(Error::UnexpectedKind),
                    tm_ty => Ok(Prod(Arg { id: id.clone(), ty: Some(ty.clone()) }, Box::new(tm_ty))),
                }
            }
            Prod(Arg { ty: Some(ty), .. }, tm) => {
                match bind(sig, ctx, *ty.clone(), |ctx| tm.infer(sig, ctx))? {
                    tm_ty @ Kind | tm_ty @ Type => Ok(tm_ty),
                    _ => Err(Error::SortExpected),
                }
            }
            Abst(Arg { ty: None, .. }, _) | Prod(Arg { ty: None, .. }, _) => {
                Err(Error::DomainFreeAbstraction)
            }
        }
    }

    pub fn check(&self, sig: &Signature, ctx: &mut Context, ty_exp: Term) -> Result<(), Error> {
        match self {
            Abst(arg, tm) => match ty_exp.whnf(sig) {
                Prod(Arg { ty: Some(ty_a), .. }, ty_b) => {
                    match arg.clone().ty {
                        None => Ok(()),
                        Some(ty_a_exp) => {
                            let _ = ty_a.infer(sig, ctx)?;
                            if reduce::convertible(sig, *ty_a_exp, *ty_a.clone()) {
                                Ok(())
                            } else {
                                Err(Error::Unconvertible)
                            }
                        }
                    }?;
                    scope::bind(ctx, Some(*ty_a), |ctx| tm.check(sig, ctx, *ty_b))
                }
                _ => Err(Error::ProductExpected),
            },
            _ => {
                let ty_inf = self.infer(sig, ctx)?;
                if reduce::convertible(sig, ty_inf, ty_exp) {
                    Ok(())
                } else {
                    Err(Error::Unconvertible)
                }
            }
        }
    }
}
