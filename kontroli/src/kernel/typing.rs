//! Type checking and type inference for terms.

use super::{GCtx, Intro, Term};
use crate::error::TypingError as Error;
use crate::Stack;

type Result<T> = core::result::Result<T, Error>;

pub type Typing<'s> = crate::Typing<Term<'s>, Option<Term<'s>>>;
pub type Check<'s> = crate::Typing<Term<'s>>;

fn declare<'s>(ty: Term<'s>, gc: &GCtx<'s>) -> Result<(Typing<'s>, Option<Check<'s>>)> {
    let ty_of_ty = ty.infer(gc, &mut Stack::new())?;
    if !matches!(ty_of_ty, Term::Kind | Term::Type) {
        return Err(Error::SortExpected);
    }
    let typing = Typing {
        lc: Stack::new(),
        ty,
        tm: None,
    };
    Ok((typing, None))
}

fn define<'s>(
    ty: Option<Term<'s>>,
    tm: Term<'s>,
    gc: &GCtx<'s>,
) -> Result<(Typing<'s>, Option<Check<'s>>)> {
    let check = ty.is_none();
    let ty = if let Some(ty) = ty {
        let _ = ty.infer(gc, &mut Stack::new())?;
        ty
    } else {
        tm.infer(gc, &mut Stack::new())?
    };
    let typing = Typing {
        lc: Stack::new(),
        ty: ty.clone(),
        tm: Some(tm.clone()),
    };
    let lc = Stack::new();
    Ok((typing, check.then(|| Check { lc, ty, tm })))
}

fn theorem<'s>(
    ty: Term<'s>,
    tm: Term<'s>,
    gc: &GCtx<'s>,
) -> Result<(Typing<'s>, Option<Check<'s>>)> {
    let _ = ty.infer(gc, &mut Stack::new())?;
    let typing = Typing {
        lc: Stack::new(),
        ty: ty.clone(),
        tm: None,
    };
    let lc = Stack::new();
    let check = Check { lc, ty, tm };
    Ok((typing, Some(check)))
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
pub fn intro<'s>(it: Intro<'s>, gc: &GCtx<'s>) -> Result<(Typing<'s>, Option<Check<'s>>)> {
    match it {
        Intro::Declaration(ty) => declare(ty, gc),
        Intro::Definition(oty, otm) => match (oty, otm) {
            (Some(ty), None) => declare(ty, gc),
            (oty, Some(tm)) => define(oty, tm, gc),
            (None, None) => Err(Error::TypeAndTermEmpty),
        },
        Intro::Theorem(ty, tm) => theorem(ty, tm, gc),
    }
}

pub fn rewrite<'s>(rule: crate::Rule<Term<'s>>, gc: &GCtx<'s>) -> Result<Check<'s>> {
    // TODO: check types in context?
    let mut lc = Stack::from(rule.ctx);
    // TODO: check for Kind/Type?
    let ty = rule.lhs.infer(gc, &mut lc)?;
    let tm = rule.rhs;
    Ok(Check { lc, ty, tm })
}

impl<'s> Check<'s> {
    /// Verify whether `t: A` if this was not previously checked.
    pub fn check(&self, gc: &GCtx<'s>) -> Result<()> {
        if self.tm.check(gc, &mut self.lc.clone(), self.ty.clone())? {
            Ok(())
        } else {
            Err(Error::Unconvertible)
        }
    }
}
