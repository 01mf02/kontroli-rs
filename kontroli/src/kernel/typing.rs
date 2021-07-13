//! Type checking and type inference for terms.

use super::{GCtx, Intro, RTerm, Term};
use crate::error::TypingError as Error;
use crate::typing::Check;
use crate::Arg;
use core::fmt;

pub type Typing<'s> = crate::Typing<RTerm<'s>>;

impl<'s> Typing<'s> {
    pub fn declare(typ: RTerm<'s>, gc: &GCtx<'s>) -> Result<Self, Error> {
        match &*typ.infer(&gc, &mut LCtx::new())? {
            Term::Kind | Term::Type => Ok(Self {
                lc: LCtx::new(),
                typ,
                term: None,
            }),
            _ => Err(Error::SortExpected),
        }
    }

    pub fn define(ty: Option<RTerm<'s>>, tm: RTerm<'s>, gc: &GCtx<'s>) -> Result<Self, Error> {
        let (typ, check) = match ty {
            None => (tm.infer(&gc, &mut LCtx::new())?, Check::Checked),
            Some(ty) => {
                let _ = ty.infer(&gc, &mut LCtx::new())?;
                (ty, Check::Unchecked)
            }
        };
        match &*typ {
            Term::Kind => Err(Error::UnexpectedKind),
            _ => Ok(Self {
                lc: LCtx::new(),
                typ,
                term: Some((tm, check)),
            }),
        }
    }

    pub fn rewrite(rule: crate::Rule<RTerm<'s>>, gc: &GCtx<'s>) -> Result<Self, Error> {
        // TODO: check types in context?
        let mut lc = LCtx::from(rule.ctx);
        // TODO: check for Kind/Type?
        Ok(Self {
            typ: rule.lhs.infer(gc, &mut lc)?,
            term: Some((rule.rhs, Check::Unchecked)),
            lc,
        })
    }

    /// Verify whether `t: A` if this was not previously checked.
    pub fn check(&self, gc: &GCtx<'s>) -> Result<(), Error> {
        if let Some((term, Check::Unchecked)) = &self.term {
            if !term.check(&gc, &mut self.lc.clone(), self.typ.clone())? {
                return Err(Error::Unconvertible);
            }
        };
        Ok(())
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
    pub fn intro(it: Intro<'s>, gc: &GCtx<'s>) -> Result<Self, Error> {
        match it {
            Intro::Declaration(ty) => Self::declare(ty, &gc),
            Intro::Definition(oty, otm) => match (oty, otm) {
                (Some(ty), None) => Self::declare(ty, &gc),
                (oty, Some(tm)) => Self::define(oty, tm, &gc),
                (None, None) => Err(Error::TypeAndTermEmpty),
            },
            Intro::Theorem(ty, tm) => Self::define(Some(ty), tm, &gc),
        }
    }
}

/// Map from de Bruijn indices to associated types.
type LCtx<'s> = crate::Stack<RTerm<'s>>;

impl<'s> LCtx<'s> {
    fn get_type(&self, n: usize) -> Option<RTerm<'s>> {
        Some(self.get(n)?.clone() << (n + 1))
    }

    fn bind<A, F>(&mut self, arg: RTerm<'s>, f: F) -> Result<A, Error>
    where
        F: FnOnce(&mut LCtx<'s>) -> Result<A, Error>,
    {
        self.try_with_pushed(arg, f)
    }

    fn bind_of_type<A, F>(&mut self, gc: &GCtx<'s>, arg: RTerm<'s>, f: F) -> Result<A, Error>
    where
        F: FnOnce(&mut LCtx<'s>) -> Result<A, Error>,
    {
        match &*arg.clone().infer(gc, self)? {
            Term::Type => self.bind(arg, f),
            _ => Err(Error::BindNoType),
        }
    }
}

impl<'s> fmt::Display for LCtx<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        for (i, x) in self.iter().enumerate() {
            write!(f, "{} : {}, ", Term::BVar(i), x.clone() << (i + 1))?;
        }
        write!(f, "]")
    }
}

impl<'s> Term<'s> {
    /// Infer the type of a term using supplied types of bound variables.
    fn infer(&self, gc: &GCtx<'s>, lc: &mut LCtx<'s>) -> Result<RTerm<'s>, Error> {
        debug!("infer type of {}", self);
        use crate::Term::*;
        match self {
            Kind => Err(Error::KindNotTypable),
            Type => Ok(RTerm::new(Kind)),
            Symb(s) => Ok(gc.types.get(&s).ok_or(Error::TypeNotFound)?.clone()),
            BVar(x) => Ok(lc.get_type(*x).ok_or(Error::TypeNotFound)?),
            Appl(tm, args) => {
                let tm_ty = tm.infer(gc, lc)?;
                args.iter().try_fold(tm_ty, |ty, arg| match &*ty.whnf(gc) {
                    Prod(Arg { ty: a, .. }, b) => {
                        if arg.check(gc, lc, a.clone())? {
                            Ok(b.clone().subst(&arg))
                        } else {
                            Err(Error::Unconvertible)
                        }
                    }
                    _ => Err(Error::ProductExpected),
                })
            }
            Abst(Arg { id, ty: Some(ty) }, tm) => {
                let tm_ty = lc.bind_of_type(gc, ty.clone(), |lc| tm.infer(gc, lc))?;
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
                let tm_ty = lc.bind_of_type(gc, ty.clone(), |lc| tm.infer(gc, lc))?;
                match &*tm_ty {
                    Kind | Type => Ok(tm_ty),
                    _ => Err(Error::SortExpected),
                }
            }
            Abst(Arg { ty: None, .. }, _) => Err(Error::DomainFreeAbstraction),
        }
    }

    /// Check whether a term is of the given type, using supplied types of bound variables.
    fn check(&self, gc: &GCtx<'s>, lc: &mut LCtx<'s>, ty_exp: RTerm<'s>) -> Result<bool, Error> {
        debug!("check {} is of type {} when {}", self, ty_exp, lc);
        use crate::Term::*;
        match self {
            Abst(arg, tm) => match &*ty_exp.whnf(gc) {
                Prod(Arg { ty: ty_a, .. }, ty_b) => {
                    let a_ok = match &arg.ty {
                        None => true,
                        Some(ty) => {
                            let _ = ty.infer(gc, lc)?;
                            RTerm::convertible(ty.clone(), ty_a.clone(), gc)
                        }
                    };
                    Ok(a_ok && lc.bind(ty_a.clone(), |lc| tm.check(gc, lc, ty_b.clone()))?)
                }
                _ => Err(Error::ProductExpected),
            },
            _ => {
                let ty_inf = self.infer(gc, lc)?;
                debug!("checking convertibility: {} ~ {}", ty_inf, ty_exp);
                Ok(RTerm::convertible(ty_inf, ty_exp, gc))
            }
        }
    }
}
