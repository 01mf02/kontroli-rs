use super::reduce::Signature;
use super::Term::*;
use super::*;

// DB -> type
type Context = Vec<Term>;

enum Error {
    ProductExpected,
    SortExpected,
    Unconvertible,
    KindNotTypable,
    UnexpectedKind,
    DomainFreeAbstraction,
}

fn bind<F, A>(sig: &Signature, ctx: &mut Context, ty: Term, f: F) -> Result<A, Error>
where
    F: FnOnce(&mut Context) -> Result<A, Error>,
{
    match ty.clone().infer(sig, ctx)? {
        Type => Ok(scope::bind(ctx, Some(ty), |ctx| f(ctx))?),
        _ => Err(Error::Unconvertible),
    }
}

impl Term {
    fn infer(self, sig: &Signature, ctx: &mut Context) -> Result<Term, Error> {
        match self {
            Kind => Err(Error::KindNotTypable),
            Type => Ok(Kind),
            Symb(s) => Ok(sig.get(&s).unwrap().clone()),
            BVar(x) => Ok(ctx.iter().rev().nth(x).unwrap().clone()),
            Appl(f, args) => {
                args.into_iter()
                    .try_fold(f.infer(sig, ctx)?, |ty, arg| match ty.whnf(sig) {
                        Prod(Arg { ty: Some(a), .. }, b) => {
                            arg.clone().check(sig, ctx, *a)?;
                            Ok(b.subst(&arg))
                        }
                        _ => Err(Error::ProductExpected),
                    })
            }
            Abst(Arg { id, ty: Some(ty) }, tm) => {
                match bind(sig, ctx, *ty.clone(), |ctx| tm.infer(sig, ctx))? {
                    Kind => Err(Error::UnexpectedKind),
                    tm_ty => Ok(Prod(Arg { id, ty: Some(ty) }, Box::new(tm_ty))),
                }
            }
            Prod(Arg { id, ty: Some(ty) }, tm) => {
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

    fn check(self, sig: &Signature, ctx: &mut Context, ty_exp: Term) -> Result<(), Error> {
        match self {
            Abst(Arg { ty: None, .. }, tm) => match ty_exp.whnf(sig) {
                Prod(Arg { ty: Some(ty_a), .. }, ty_b) => {
                    scope::bind(ctx, Some(*ty_a), |ctx| tm.check(sig, ctx, *ty_b))
                }
                _ => Err(Error::ProductExpected),
            },
            Abst(
                Arg {
                    ty: Some(ty_a_exp), ..
                },
                tm,
            ) => {
                match ty_exp.whnf(sig) {
                    Prod(Arg { ty: Some(ty_a), .. }, ty_b) => {
                        let _ = ty_a.clone().infer(sig, ctx);
                        if !reduce::convertible(sig, *ty_a_exp.clone(), *ty_a) {
                            Err(Error::Unconvertible)
                        } else {
                            // TODO: can we use ty_a instead of ty_a_exp here?
                            scope::bind(ctx, Some(*ty_a_exp), |ctx| tm.check(sig, ctx, *ty_b))
                        }
                    }
                    _ => Err(Error::ProductExpected),
                }
            }
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
