use super::reduce::Signature;
use super::Term::*;
use super::*;

// DB -> type
type Context = Vec<Term>;

fn bind<F, A>(sig: &Signature, ctx: &mut Context, ty: Term, f: F) -> A
where
    F: FnOnce(&mut Context) -> A,
{
    match infer(sig, ctx, ty.clone()) {
        Type => scope::bind(ctx, Some(ty), |ctx| f(ctx)),
        _ => panic!("convertibility error"),
    }
}

fn infer(sig: &Signature, ctx: &mut Context, tm: Term) -> Term {
    match tm {
        Kind => panic!("kind is not typable"),
        Type => Kind,
        Symb(s) => sig.get(&s).unwrap().clone(),
        BVar(x) => ctx.iter().rev().nth(x).unwrap().clone(),
        Abst((id, Some(ty)), tm) => match bind(sig, ctx, *ty.clone(), |ctx| infer(sig, ctx, *tm)) {
            Kind => panic!("unexpected kind"),
            tm_ty => Prod((id, Some(ty)), Box::new(tm_ty)),
        },
        Prod((id, Some(ty)), tm) => match bind(sig, ctx, *ty.clone(), |ctx| infer(sig, ctx, *tm)) {
            tm_ty @ Kind | tm_ty @ Type => tm_ty,
            _ => panic!("sort expected"),
        },
        Abst((_, None), _) | Prod((_, None), _) => panic!("domain-free lambda/pi"),
        _ => panic!("todo"),
    }
}

fn check(sig: &Signature, ctx: &mut Context, tm: Term, ty_exp: Term) {
    match tm {
        Abst((id, None), tm) => match ty_exp.whnf(sig) {
            Prod((_, Some(ty_a)), ty_b) => {
                scope::bind(ctx, Some(*ty_a), |ctx| check(sig, ctx, *tm, *ty_b))
            }
            _ => panic!("product expected"),
        },
        Abst((id, Some(ty_a_exp)), tm) => {
            match ty_exp.whnf(sig) {
                Prod((_, Some(ty_a)), ty_b) => {
                    let _ = infer(sig, ctx, *ty_a.clone());
                    if !reduce::convertible(sig, *ty_a_exp.clone(), *ty_a) {
                        panic!("convertibility error")
                    } else {
                        // TODO: can we use ty_a instead of ty_a_exp here?
                        scope::bind(ctx, Some(*ty_a_exp), |ctx| check(sig, ctx, *tm, *ty_b))
                    }
                }
                _ => panic!("product expected"),
            }
        }
        _ => panic!("todo"),
    }
}
