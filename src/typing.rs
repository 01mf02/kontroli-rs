use super::Term::*;
use super::*;
use crate::signature::Signature;

// DB -> type
#[derive(Debug)]
pub struct Context(Vec<Term>);

impl Context {
    pub fn new() -> Self {
        Context(Vec::new())
    }

    fn get_type(&self, n: usize) -> Option<Term> {
        Some(self.0.iter().rev().nth(n)?.clone() << (n + 1))
    }

    fn bind<A, F>(&mut self, arg: Term, f: F) -> Result<A, Error>
    where
        F: FnOnce(&mut Context) -> Result<A, Error>,
    {
        self.0.push(arg);
        let x = f(self)?;
        self.0.pop();
        Ok(x)
    }

    fn bind_of_type<A, F>(&mut self, sig: &Signature, arg: Term, f: F) -> Result<A, Error>
    where
        F: FnOnce(&mut Context) -> Result<A, Error>,
    {
        match arg.infer(sig, self)? {
            Type => self.bind(arg, f),
            _ => Err(Error::BindNoType),
        }
    }
}

impl std::fmt::Display for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[")?;
        for (i, x) in self.0.iter().rev().enumerate() {
            write!(f, "{} : {}, ", BVar(i), x.clone() << (i + 1))?;
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

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "typing error")
    }
}

impl std::error::Error for Error {}

fn assert_convertible(sig: &Signature, tm1: Term, tm2: Term) -> Result<(), Error> {
    if reduce::convertible(sig, tm1, tm2) {
        Ok(())
    } else {
        Err(Error::Unconvertible)
    }
}

impl Term {
    pub fn infer(&self, sig: &Signature, ctx: &mut Context) -> Result<Term, Error> {
        debug!("infer type of {}", self);
        match self {
            Kind => Err(Error::KindNotTypable),
            Type => Ok(Kind),
            Symb(s) => Ok(sig.get(&s).unwrap().typ.clone()),
            BVar(x) => Ok(ctx.get_type(*x).unwrap()),
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
                match ctx.bind_of_type(sig, *ty.clone(), |ctx| tm.infer(sig, ctx))? {
                    Kind => Err(Error::UnexpectedKind),
                    tm_ty => Ok(Prod(
                        Arg {
                            id: id.clone(),
                            ty: Some(ty.clone()),
                        },
                        Box::new(tm_ty),
                    )),
                }
            }
            Prod(Arg { ty: Some(ty), .. }, tm) => {
                match ctx.bind_of_type(sig, *ty.clone(), |ctx| tm.infer(sig, ctx))? {
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
        debug!("check {} is of type {} when {}", self, ty_exp, ctx);
        match self {
            Abst(arg, tm) => match ty_exp.whnf(sig) {
                Prod(Arg { ty: Some(ty_a), .. }, ty_b) => {
                    match arg.ty.clone() {
                        None => Ok(()),
                        Some(ty_a_exp) => {
                            let _ = ty_a_exp.infer(sig, ctx)?;
                            assert_convertible(sig, *ty_a_exp, *ty_a.clone())
                        }
                    }?;
                    ctx.bind(*ty_a, |ctx| tm.check(sig, ctx, *ty_b))
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
