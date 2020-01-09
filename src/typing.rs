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
    match infer(sig, ctx, ty.clone())? {
        Type => Ok(scope::bind(ctx, Some(ty), |ctx| f(ctx))?),
        _ => Err(Error::Unconvertible),
    }
}

fn infer(sig: &Signature, ctx: &mut Context, tm: Term) -> Result<Term, Error> {
    match tm {
        Kind => Err(Error::KindNotTypable),
        Type => Ok(Kind),
        Symb(s) => Ok(sig.get(&s).unwrap().clone()),
        BVar(x) => Ok(ctx.iter().rev().nth(x).unwrap().clone()),
        Appl(f, args) => {
            args.into_iter()
                .try_fold(infer(sig, ctx, *f)?, |ty, arg| match ty.whnf(sig) {
                    Prod(Arg { ty: Some(a), .. }, b) => {
                        check(sig, ctx, *arg.clone(), *a)?;
                        Ok(b.subst(&arg))
                    }
                    _ => Err(Error::ProductExpected),
                })
        }
        Abst(Arg { id, ty: Some(ty) }, tm) => {
            match bind(sig, ctx, *ty.clone(), |ctx| infer(sig, ctx, *tm))? {
                Kind => Err(Error::UnexpectedKind),
                tm_ty => Ok(Prod(Arg { id, ty: Some(ty) }, Box::new(tm_ty))),
            }
        }
        Prod(Arg { id, ty: Some(ty) }, tm) => {
            match bind(sig, ctx, *ty.clone(), |ctx| infer(sig, ctx, *tm))? {
                tm_ty @ Kind | tm_ty @ Type => Ok(tm_ty),
                _ => Err(Error::SortExpected),
            }
        }
        Abst(Arg { ty: None, .. }, _) | Prod(Arg { ty: None, .. }, _) => {
            Err(Error::DomainFreeAbstraction)
        }
    }
}

fn check(sig: &Signature, ctx: &mut Context, tm: Term, ty_exp: Term) -> Result<(), Error> {
    match tm {
        Abst(Arg { ty: None, .. }, tm) => match ty_exp.whnf(sig) {
            Prod(Arg { ty: Some(ty_a), .. }, ty_b) => {
                scope::bind(ctx, Some(*ty_a), |ctx| check(sig, ctx, *tm, *ty_b))
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
                    let _ = infer(sig, ctx, *ty_a.clone());
                    if !reduce::convertible(sig, *ty_a_exp.clone(), *ty_a) {
                        Err(Error::Unconvertible)
                    } else {
                        // TODO: can we use ty_a instead of ty_a_exp here?
                        scope::bind(ctx, Some(*ty_a_exp), |ctx| check(sig, ctx, *tm, *ty_b))
                    }
                }
                _ => Err(Error::ProductExpected),
            }
        }
        _ => {
            let ty_inf = infer(sig, ctx, tm)?;
            if reduce::convertible(sig, ty_inf, ty_exp) {
                Ok(())
            } else {
                Err(Error::Unconvertible)
            }
        }
    }
}
