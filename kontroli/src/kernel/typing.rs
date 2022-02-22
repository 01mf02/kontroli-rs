//! Type checking and type inference for terms.

use super::{GCtx, Intro, RTerm, Term, TermC, Typing};
use crate::error::TypingError as Error;
use crate::typing::Check;
use crate::Arg;
use core::fmt;

type Result<T> = core::result::Result<T, Error>;

impl<'s> Typing<'s> {
    pub fn declare(ty: Term<'s>, gc: &GCtx<'s>) -> Result<Self> {
        match ty.infer(gc, &mut LCtx::new())? {
            Term::Kind | Term::Type => Ok(Self {
                lc: LCtx::new(),
                ty,
                tm: None,
            }),
            _ => Err(Error::SortExpected),
        }
    }

    pub fn define(typ: Option<Term<'s>>, tm: Term<'s>, gc: &GCtx<'s>) -> Result<Self> {
        let (ty, check) = match typ {
            None => (tm.infer(gc, &mut LCtx::new())?, Check::Checked),
            Some(ty) => {
                let _ = ty.infer(gc, &mut LCtx::new())?;
                (ty, Check::Unchecked)
            }
        };
        match ty {
            Term::Kind => Err(Error::UnexpectedKind),
            _ => Ok(Self {
                lc: LCtx::new(),
                ty,
                tm: Some((tm, check)),
            }),
        }
    }

    pub fn rewrite(rule: crate::Rule<Term<'s>>, gc: &GCtx<'s>) -> Result<Self> {
        // TODO: check types in context?
        let mut lc = LCtx::from(rule.ctx);
        // TODO: check for Kind/Type?
        Ok(Self {
            ty: rule.lhs.infer(gc, &mut lc)?,
            tm: Some((rule.rhs, Check::Unchecked)),
            lc,
        })
    }

    /// Verify whether `t: A` if this was not previously checked.
    pub fn check(&self, gc: &GCtx<'s>) -> Result<()> {
        if let Some((term, Check::Unchecked)) = &self.tm {
            if !term.check(gc, &mut self.lc.clone(), self.ty.clone())? {
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
    pub fn intro(it: Intro<'s>, gc: &GCtx<'s>) -> Result<Self> {
        match it {
            Intro::Declaration(ty) => Self::declare(ty, gc),
            Intro::Definition(oty, otm) => match (oty, otm) {
                (Some(ty), None) => Self::declare(ty, gc),
                (oty, Some(tm)) => Self::define(oty, tm, gc),
                (None, None) => Err(Error::TypeAndTermEmpty),
            },
            Intro::Theorem(ty, tm) => Self::define(Some(ty), tm, gc),
        }
    }
}

/// Map from de Bruijn indices to associated types.
type LCtx<'s> = crate::Stack<Term<'s>>;

impl<'s> LCtx<'s> {
    fn get_type(&self, n: usize) -> Option<Term<'s>> {
        Some(self.get(n)?.clone() << (n + 1))
    }

    fn bind<A, F>(&mut self, arg: Term<'s>, f: F) -> Result<A>
    where
        F: FnOnce(&mut LCtx<'s>) -> Result<A>,
    {
        self.try_with_pushed(arg, f)
    }

    fn bind_of_type<A, F>(&mut self, gc: &GCtx<'s>, arg: Term<'s>, f: F) -> Result<A>
    where
        F: FnOnce(&mut LCtx<'s>) -> Result<A>,
    {
        match arg.clone().infer(gc, self)? {
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
    fn infer(&self, gc: &GCtx<'s>, lc: &mut LCtx<'s>) -> Result<Term<'s>> {
        debug!("infer type of {}", self);
        use crate::Term::*;
        match self {
            Kind => Err(Error::KindNotTypable),
            Type => Ok(Kind),
            Symb(s) => Ok(gc.types.get(s).ok_or(Error::TypeNotFound)?.clone()),
            BVar(x) => Ok(lc.get_type(*x).ok_or(Error::TypeNotFound)?),
            Comb(c) => c.infer(gc, lc),
        }
    }

    /// Check whether a term is of the given type, using supplied types of bound variables.
    fn check(&self, gc: &GCtx<'s>, lc: &mut LCtx<'s>, ty_exp: Term<'s>) -> Result<bool> {
        debug!("check {} is of type {} when {}", self, ty_exp, lc);
        if let Some((arg, tm)) = self.get_abst() {
            let whnf = ty_exp.whnf(gc);
            let (Arg { ty: ty_a, .. }, ty_b) = whnf.get_prod().ok_or(Error::ProductExpected)?;
            let a_ok = match &arg.ty {
                None => true,
                Some(ty) => {
                    let _ = ty.infer(gc, lc)?;
                    Term::convertible(ty.clone(), ty_a.clone(), gc)
                }
            };
            Ok(a_ok && lc.bind(ty_a.clone(), |lc| tm.check(gc, lc, ty_b.clone()))?)
        } else {
            let ty_inf = self.infer(gc, lc)?;
            debug!("checking convertibility: {} ~ {}", ty_inf, ty_exp);
            Ok(Term::convertible(ty_inf, ty_exp, gc))
        }
    }
}

impl<'s> TermC<'s> {
    fn infer(&self, gc: &GCtx<'s>, lc: &mut LCtx<'s>) -> Result<Term<'s>> {
        use crate::term::{Term::*, TermC::*};
        match self {
            Appl(tm, args) => {
                let tm_ty = tm.infer(gc, lc)?;
                args.iter().try_fold(tm_ty, |ty, arg| {
                    let whnf = ty.whnf(gc);
                    let (Arg { ty: a, .. }, b) = whnf.get_prod().ok_or(Error::ProductExpected)?;
                    if arg.check(gc, lc, a.clone())? {
                        Ok(b.clone().subst(arg))
                    } else {
                        Err(Error::Unconvertible)
                    }
                })
            }
            Abst(Arg { id, ty: Some(ty) }, tm) => {
                let tm_ty = lc.bind_of_type(gc, ty.clone(), |lc| tm.infer(gc, lc))?;
                if tm_ty == Kind {
                    Err(Error::UnexpectedKind)
                } else {
                    let id = id.clone();
                    let ty = ty.clone();
                    Ok(Comb(RTerm::new(Prod(Arg { id, ty }, tm_ty))))
                }
            }
            Prod(Arg { ty, .. }, tm) => {
                let tm_ty = lc.bind_of_type(gc, ty.clone(), |lc| tm.infer(gc, lc))?;
                match tm_ty {
                    Kind | Type => Ok(tm_ty),
                    _ => Err(Error::SortExpected),
                }
            }
            Abst(Arg { ty: None, .. }, _) => Err(Error::DomainFreeAbstraction),
        }
    }
}
