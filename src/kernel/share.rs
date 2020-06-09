//! Convert from scoped to shared structures.

use super::command::{Command, IntroType};
use super::pattern::{Pattern, TopPattern};
use super::term::{Arg, RTerm, Term};
use super::{Rc, Rule};
use crate::scope::command::{Command as SCommand, IntroType as SIntroType};
use crate::scope::pattern::{Pattern as SPattern, TopPattern as STopPattern};
use crate::scope::term::{Arg as SArg, RTerm as SRTerm, Term as STerm};
use crate::scope::Rule as SRule;

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
            STerm::Abst(arg, tm) => Self::Abst(Arg::from(arg), RTerm::from(tm)),
            STerm::Prod(arg, tm) => Self::Prod(Arg::from(arg), RTerm::from(tm)),
        }
    }
}

impl<'s> From<SArg<'s>> for Arg<'s> {
    fn from(arg: SArg<'s>) -> Self {
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

impl<'s> From<STopPattern<'s>> for TopPattern<'s> {
    fn from(rule: STopPattern<'s>) -> Self {
        Self {
            symbol: rule.symbol,
            args: rule.args.into_iter().map(Pattern::from).collect(),
        }
    }
}

impl<'s> From<SPattern<'s>> for Pattern<'s> {
    fn from(rule: SPattern<'s>) -> Self {
        match rule {
            SPattern::Symb(s, pats) => Self::Symb(s, pats.into_iter().map(Self::from).collect()),
            SPattern::MVar(m) => Self::MVar(m),
            SPattern::Joker => Self::Joker,
        }
    }
}

impl<'s> From<SRule<'s>> for Rule<'s> {
    fn from(rule: SRule<'s>) -> Self {
        Self {
            ctx: rule.ctx,
            lhs: TopPattern::from(rule.lhs),
            rhs: RTerm::from(rule.rhs),
        }
    }
}

impl<'s> From<SIntroType<'s>> for IntroType<'s> {
    fn from(cmd: SIntroType<'s>) -> Self {
        cmd.map_type(RTerm::from).map_term(RTerm::from)
    }
}

impl<'s> From<SCommand<'s>> for Command<'s> {
    fn from(cmd: SCommand<'s>) -> Self {
        match cmd {
            SCommand::Intro(id, it) => Self::Intro(id, IntroType::from(it)),
            SCommand::Rule(rule) => Self::Rule(Rule::from(rule)),
        }
    }
}
