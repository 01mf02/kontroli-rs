use alloc::vec::Vec;
use core::fmt::{self, Display};

/// Rewrite rule.
#[derive(Clone, Debug)]
pub struct Rule<V, L = V, R = L> {
    /// context (bound variables)
    pub ctx: Vec<V>,
    /// left-hand side (pattern to match with)
    pub lhs: L,
    /// right-hand side (term to replace with)
    pub rhs: R,
}

#[derive(Debug)]
pub enum Error {
    TypeAnnotation,
    PatternNoTerm,
}

impl<V, L, R> Rule<V, L, R> {
    pub fn map_lhs<L2>(self, f: impl FnOnce(L) -> L2) -> Rule<V, L2, R> {
        Rule {
            ctx: self.ctx,
            lhs: f(self.lhs),
            rhs: self.rhs,
        }
    }
}

impl<V, Pat, Tm: TryFrom<Pat>> TryFrom<Rule<(V, Option<Tm>), Pat, Tm>> for Rule<Tm> {
    type Error = Error;
    fn try_from(rule: Rule<(V, Option<Tm>), Pat, Tm>) -> Result<Self, Self::Error> {
        let ctx = rule.ctx.into_iter().map(|arg| arg.1);
        let ctx = ctx.collect::<Option<_>>().ok_or(Error::TypeAnnotation)?;
        let lhs = Tm::try_from(rule.lhs).map_err(|_| Error::PatternNoTerm)?;
        let rhs = rule.rhs;
        Ok(Self { ctx, lhs, rhs })
    }
}

impl<V, L: Display, R: Display> Display for Rule<V, L, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ⟶ {}", self.lhs, self.rhs)
    }
}
