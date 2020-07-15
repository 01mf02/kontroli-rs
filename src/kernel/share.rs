//! Convert from scoped to shared structures.

use super::rterm::{Arg, OptArg, RTerm};
use super::{Intro, Rc, Rule, Term};
use crate::scope::rterm::{Arg as SArg, OptArg as SOptArg, RTerm as SRTerm};
use crate::scope::{Intro as SIntro, Rule as SRule, Term as STerm};

impl<'s> From<STerm<'s>> for Term<'s> {
    fn from(tm: STerm<'s>) -> Self {
        match tm {
            STerm::Kind => Self::Kind,
            STerm::Type => Self::Type,
            STerm::Symb(s) => Self::Symb(s),
            STerm::BVar(b) => Self::BVar(b),
            STerm::Appl(tm, args) => {
                Self::Appl(RTerm::from(tm), args.into_iter().map(RTerm::from).collect())
            }
            STerm::Abst(arg, tm) => Self::Abst(OptArg::from(arg), RTerm::from(tm)),
            STerm::Prod(arg, tm) => Self::Prod(Arg::from(arg), RTerm::from(tm)),
        }
    }
}

impl<'s> From<SArg<'s>> for Arg<'s> {
    fn from(arg: SArg<'s>) -> Self {
        Self {
            id: Rc::new(arg.id),
            ty: RTerm::from(arg.ty),
        }
    }
}

impl<'s> From<SOptArg<'s>> for OptArg<'s> {
    fn from(arg: SOptArg<'s>) -> Self {
        Self {
            id: Rc::new(arg.id),
            ty: arg.ty.map(RTerm::from),
        }
    }
}

impl<'s> From<SRTerm<'s>> for RTerm<'s> {
    fn from(tm: SRTerm<'s>) -> Self {
        Self::new(Term::from(*tm))
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
