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
        Abst((id, Some(ty)), tm) => {
            match bind(sig, ctx, *ty.clone(), |ctx| infer(sig, ctx, *tm))? {
                Kind => Err(Error::UnexpectedKind),
                tm_ty => Ok(Prod((id, Some(ty)), Box::new(tm_ty))),
            }
        }
        Prod((id, Some(ty)), tm) => {
            match bind(sig, ctx, *ty.clone(), |ctx| infer(sig, ctx, *tm))? {
                tm_ty @ Kind | tm_ty @ Type => Ok(tm_ty),
                _ => Err(Error::SortExpected),
            }
        }
        Abst((_, None), _) | Prod((_, None), _) => Err(Error::DomainFreeAbstraction),
        _ => unimplemented!(),
    }
}

fn check(sig: &Signature, ctx: &mut Context, tm: Term, ty_exp: Term) -> Result<(), Error> {
    match tm {
        Abst((id, None), tm) => match ty_exp.whnf(sig) {
            Prod((_, Some(ty_a)), ty_b) => {
                scope::bind(ctx, Some(*ty_a), |ctx| check(sig, ctx, *tm, *ty_b))
            }
            _ => Err(Error::ProductExpected),
        },
        Abst((id, Some(ty_a_exp)), tm) => {
            match ty_exp.whnf(sig) {
                Prod((_, Some(ty_a)), ty_b) => {
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
