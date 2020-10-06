//! Convert from scoped to shared structures.

use super::{Intro, RTerm, Rc, Rule, Term};
use crate::scope::{Intro as SIntro, RTerm as SRTerm, Rule as SRule, Term as STerm};

impl<'s> From<STerm<'s>> for Term<'s> {
    fn from(tm: STerm<'s>) -> Self {
        tm.map(Rc::new, &RTerm::from)
    }
}

impl<'s> From<SRTerm<'s>> for RTerm<'s> {
    fn from(tm: SRTerm<'s>) -> Self {
        Self::new(Term::from(tm.get()))
    }
}

impl<'s> From<SRule<'s>> for Rule<'s> {
    fn from(rule: SRule<'s>) -> Self {
        Self {
            ctx: rule.ctx,
            lhs: rule.lhs,
            rhs: RTerm::from(rule.rhs),
        }
    }
}

impl<'s> From<SIntro<'s>> for Intro<'s> {
    fn from(cmd: SIntro<'s>) -> Self {
        cmd.map_type(RTerm::from).map_term(RTerm::from)
    }
}
