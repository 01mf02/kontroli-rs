//! Type checking and type inference for terms.

use super::{Intro, RTerm, Signature, Term};
use crate::error::TypingError as Error;
use crate::typing::Check;
use crate::Arg;
use core::fmt;

pub type Typing<'s> = crate::Typing<RTerm<'s>>;

impl<'s> Typing<'s> {
    pub fn declare(typ: RTerm<'s>, rewritable: bool, sig: &Signature<'s>) -> Result<Self, Error> {
        match &*typ.infer(&sig)? {
            Term::Kind | Term::Type => Ok(Self {
                rewritable,
                typ,
                term: None,
            }),
            _ => Err(Error::SortExpected),
        }
    }

    pub fn define(
        oty: Option<RTerm<'s>>,
        term: RTerm<'s>,
        rewritable: bool,
        sig: &Signature<'s>,
    ) -> Result<Self, Error> {
        let (typ, check) = match oty {
            None => (term.infer(&sig)?, Check::Checked),
            Some(ty) => {
                let _ = ty.infer(&sig)?;
                (ty, Check::Unchecked)
            }
        };
        match &*typ {
            Term::Kind => Err(Error::UnexpectedKind),
            _ => Ok(Self {
                typ,
                term: Some((term, check)),
                rewritable,
            }),
        }
    }

    /// Verify whether `t: A` if this was not previously checked.
    ///
    /// Return a typing registering that `t: A` has been checked.
    pub fn check(mut self, sig: &Signature<'s>) -> Result<Self, Error> {
        if let Some((term, Check::Unchecked)) = self.term {
            if term.check(&sig, self.typ.clone())? {
                self.term = Some((term, Check::Checked));
            } else {
                return Err(Error::Unconvertible);
            }
        };
        Ok(self)
    }

    /// Construct a typing from an introduction command.
    ///
    /// An introduction command can have many shapes, such as
    /// `x: A`, `x := t`, `x: A := t`, ...
    /// The type `A` of the newly introduced symbol is
    /// (a) inferred from its defining term `t` if not given and
    /// (b) verified to be of a proper sort.
    /// In case a defining term `t` is given, `t` is registered,
    /// along with the information whether the type `A` was inferred from it.
    ///
    /// Constructing a typing from a command of the shape `x: A := t`
    /// does *not* check whether `t: A`. For this, the `check` function can be used.
    /// This allows us to postpone and parallelise type checking.
    pub fn new(it: Intro<'s>, sig: &Signature<'s>) -> Result<Self, Error> {
        match it {
            Intro::Declaration(ty) => Self::declare(ty, false, &sig),
            Intro::Definition(oty, otm) => match (oty, otm) {
                (Some(ty), None) => Self::declare(ty, true, &sig),
                (oty, Some(tm)) => Self::define(oty, tm, true, &sig),
                (None, None) => Err(Error::TypeAndTermEmpty),
            },
            Intro::Theorem(ty, tm) => Self::define(Some(ty), tm, false, &sig),
        }
    }
}

/// Map from de Bruijn indices to associated types.
type Context<'s> = crate::Stack<RTerm<'s>>;

impl<'s> Context<'s> {
    fn get_type(&self, n: usize) -> Option<RTerm<'s>> {
        Some(self.get(n)?.clone() << (n + 1))
    }

    fn bind<A, F>(&mut self, arg: RTerm<'s>, f: F) -> Result<A, Error>
    where
        F: FnOnce(&mut Context<'s>) -> Result<A, Error>,
    {
        self.with_pushed(arg, f)
    }

    fn bind_of_type<A, F>(&mut self, sig: &Signature<'s>, arg: RTerm<'s>, f: F) -> Result<A, Error>
    where
        F: FnOnce(&mut Context<'s>) -> Result<A, Error>,
    {
        match &*arg.clone().infern(sig, self)? {
            Term::Type => self.bind(arg, f),
            _ => Err(Error::BindNoType),
        }
    }
}

impl<'s> fmt::Display for Context<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        for (i, x) in self.iter().enumerate() {
            write!(f, "{} : {}, ", Term::BVar(i), x.clone() << (i + 1))?;
        }
        write!(f, "]")
    }
}

impl<'s> Term<'s> {
    /// Infer the type of a closed term.
    pub fn infer(&self, sig: &Signature<'s>) -> Result<RTerm<'s>, Error> {
        self.infern(sig, &mut Context::new())
    }

    /// Check whether a closed term is of a given type.
    pub fn check(&self, sig: &Signature<'s>, ty_exp: RTerm<'s>) -> Result<bool, Error> {
        self.checkn(sig, &mut Context::new(), ty_exp)
    }

    /// Infer the type of an open term using supplied types of bound variables.
    fn infern(&self, sig: &Signature<'s>, ctx: &mut Context<'s>) -> Result<RTerm<'s>, Error> {
        debug!("infer type of {}", self);
        use crate::Term::*;
        match self {
            Kind => Err(Error::KindNotTypable),
            Type => Ok(RTerm::new(Kind)),
            Symb(s) => Ok(sig.types.get(&s).ok_or(Error::TypeNotFound)?.clone()),
            BVar(x) => Ok(ctx.get_type(*x).ok_or(Error::TypeNotFound)?),
            Appl(f, args) => {
                args.iter()
                    .try_fold(f.infern(sig, ctx)?, |ty, arg| match &*ty.whnf(sig) {
                        Prod(Arg { ty: a, .. }, b) => {
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
                        let ty = ty.clone();
                        Ok(RTerm::new(Prod(Arg { id, ty }, tm_ty)))
                    }
                }
            }
            Prod(Arg { ty, .. }, tm) => {
                let tm_ty = ctx.bind_of_type(sig, ty.clone(), |ctx| tm.infern(sig, ctx))?;
                match &*tm_ty {
                    Kind | Type => Ok(tm_ty),
                    _ => Err(Error::SortExpected),
                }
            }
            Abst(Arg { ty: None, .. }, _) => Err(Error::DomainFreeAbstraction),
        }
    }

    /// Check whether an open term is of the given type,
    /// using supplied types of bound variables.
    fn checkn(
        &self,
        sig: &Signature<'s>,
        ctx: &mut Context<'s>,
        ty_exp: RTerm<'s>,
    ) -> Result<bool, Error> {
        debug!("check {} is of type {} when {}", self, ty_exp, ctx);
        use crate::Term::*;
        match self {
            Abst(arg, tm) => match &*ty_exp.whnf(sig) {
                Prod(Arg { ty: ty_a, .. }, ty_b) => {
                    let a_ok = match &arg.ty {
                        None => true,
                        Some(ty) => {
                            let _ = ty.infern(sig, ctx)?;
                            RTerm::convertible(ty.clone(), ty_a.clone(), sig)
                        }
                    };
                    Ok(a_ok && ctx.bind(ty_a.clone(), |ctx| tm.checkn(sig, ctx, ty_b.clone()))?)
                }
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
