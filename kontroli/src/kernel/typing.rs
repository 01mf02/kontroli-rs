//! Type checking and type inference for terms.

use super::{GCtx, Intro, Term, Typing};
use crate::error::TypingError as Error;
use crate::typing::Check;
use crate::Stack;

type Result<T> = core::result::Result<T, Error>;

fn declare<'s>(ty: Term<'s>, gc: &GCtx<'s>) -> Result<Typing<'s>> {
    match ty.infer(gc, &mut Stack::new())? {
        Term::Kind | Term::Type => Ok(Typing {
            lc: Stack::new(),
            ty,
            tm: None,
        }),
        _ => Err(Error::SortExpected),
    }
}

fn define<'s>(typ: Option<Term<'s>>, tm: Term<'s>, gc: &GCtx<'s>) -> Result<Typing<'s>> {
    let (ty, check) = match typ {
        None => (tm.infer(gc, &mut Stack::new())?, Check::Checked),
        Some(ty) => {
            let _ = ty.infer(gc, &mut Stack::new())?;
            (ty, Check::Unchecked)
        }
    };
    match ty {
        Term::Kind => Err(Error::UnexpectedKind),
        _ => Ok(Typing {
            lc: Stack::new(),
            ty,
            tm: Some((tm, check)),
        }),
    }
}

pub fn rewrite<'s>(rule: crate::Rule<Term<'s>>, gc: &GCtx<'s>) -> Result<Typing<'s>> {
    // TODO: check types in context?
    let mut lc = Stack::from(rule.ctx);
    // TODO: check for Kind/Type?
    Ok(Typing {
        ty: rule.lhs.infer(gc, &mut lc)?,
        tm: Some((rule.rhs, Check::Unchecked)),
        lc,
    })
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
pub fn intro<'s>(it: Intro<'s>, gc: &GCtx<'s>) -> Result<Typing<'s>> {
    match it {
        Intro::Declaration(ty) => declare(ty, gc),
        Intro::Definition(oty, otm) => match (oty, otm) {
            (Some(ty), None) => declare(ty, gc),
            (oty, Some(tm)) => define(oty, tm, gc),
            (None, None) => Err(Error::TypeAndTermEmpty),
        },
        Intro::Theorem(ty, tm) => define(Some(ty), tm, gc),
    }
}

impl<'s> Typing<'s> {
    /// Verify whether `t: A` if this was not previously checked.
    pub fn check(&self, gc: &GCtx<'s>) -> Result<()> {
        if let Some((term, Check::Unchecked)) = &self.tm {
            if !term.check(gc, &mut self.lc.clone(), self.ty.clone())? {
                return Err(Error::Unconvertible);
            }
        };
        Ok(())
    }
}
