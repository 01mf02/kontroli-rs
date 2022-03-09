//! Type checking kernel.

mod convertible;
mod infer_check;
mod reduce;
mod sterm;
mod subst;
mod tests;

use crate::error::TypingError as Error;
use crate::share::{Intro, Pattern, Rule, TopPattern};
use sterm::{LTerm, STerm};

pub type GCtx<'s> = crate::GCtx<crate::Symbol<'s>, Pattern<'s>, LTerm<'s>>;
pub type Typing<'s> = crate::Typing<LTerm<'s>, Option<LTerm<'s>>>;
pub type Check<'s> = crate::Typing<LTerm<'s>>;

type Result<T> = core::result::Result<T, Error>;
type IntroResult<'s> = Result<(Typing<'s>, Option<Check<'s>>)>;

impl From<sterm::UnexpectedKind> for Error {
    fn from(_: sterm::UnexpectedKind) -> Self {
        Error::UnexpectedKind
    }
}

fn declare<'s>(ty: LTerm<'s>, gc: &GCtx<'s>) -> IntroResult<'s> {
    let ty_of_ty = STerm::from(&ty).infer(gc, &mut Default::default())?;
    if !matches!(ty_of_ty, STerm::Kind | STerm::Type) {
        return Err(Error::SortExpected);
    }
    Ok((Typing::new(ty, None), None))
}

fn define<'s>(ty: Option<LTerm<'s>>, tm: LTerm<'s>, gc: &GCtx<'s>) -> IntroResult<'s> {
    let check = ty.is_some();
    let ty = if let Some(ty) = ty {
        let _ = STerm::from(&ty).infer(gc, &mut Default::default())?;
        ty
    } else {
        LTerm::try_from(&STerm::from(&tm).infer(gc, &mut Default::default())?)?
    };
    let check = check.then(|| Check::new(ty.clone(), tm.clone()));
    Ok((Typing::new(ty, Some(tm)), check))
}

fn theorem<'s>(ty: LTerm<'s>, tm: LTerm<'s>, gc: &GCtx<'s>) -> IntroResult<'s> {
    let _ = STerm::from(&ty).infer(gc, &mut Default::default())?;
    Ok((Typing::new(ty.clone(), None), Some(Check::new(ty, tm))))
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
pub fn intro<'s>(it: Intro<'s>, gc: &GCtx<'s>) -> IntroResult<'s> {
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

pub fn rewrite<'s>(rule: crate::Rule<LTerm<'s>>, gc: &GCtx<'s>) -> Result<Check<'s>> {
    // TODO: check types in context?
    let mut lc = rule.ctx.iter().map(STerm::from).collect();

    Ok(Check {
        tm: rule.rhs,
        // TODO: check for Kind/Type?
        ty: LTerm::try_from(&STerm::from(&rule.lhs).infer(gc, &mut lc)?)?,
        lc: rule.ctx.clone(),
    })
}

impl<'s> Check<'s> {
    /// Verify whether `t: A`.
    pub fn check(self, gc: &GCtx<'s>) -> Result<()> {
        let mut lc = self.lc.iter().map(STerm::from).collect();
        let tm = STerm::from(&self.tm);
        let ty = STerm::from(&self.ty);
        if tm.check(gc, &mut lc, ty)? {
            Ok(())
        } else {
            Err(Error::Unconvertible)
        }
    }
}
