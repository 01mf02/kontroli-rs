//! Conversion from preterms to terms, from prepatterns to prepatterns etc.

use super::command::{Command, IntroType};
use super::pattern::{Pattern, TopPattern};
use super::term::{Arg, RTerm, Term};
use super::{Rule, Symbol, Symbols};
use crate::error::ScopeError as Error;
use crate::pre;
use crate::pre::command::IntroType as PreIntroType;
use crate::pre::term::{Arg as Prearg, Binder};
use crate::stack::Stack;
use alloc::{string::String, string::ToString};
use core::convert::TryFrom;

type Bound = Stack<String>;

impl<'s> Term<'s> {
    fn scoper(tm: pre::Term, syms: &Symbols<'s>, bnd: &mut Bound) -> Result<RTerm<'s>, Error> {
        Ok(RTerm::new(Self::scopen(tm, syms, bnd)?))
    }

    /// Scope an open preterm using supplied bound variables.
    fn scopen(tm: pre::Term, syms: &Symbols<'s>, bnd: &mut Bound) -> Result<Self, Error> {
        match tm {
            pre::Term::Symb(s) => {
                if s == "_" {
                    Err(Error::Underscore)
                } else if s == "Type" {
                    Ok(Self::Type)
                } else if let Some(idx) = bnd.iter().position(|id| *id == *s) {
                    Ok(Self::BVar(idx))
                } else {
                    let entry = syms.get(&s).ok_or(Error::UndeclaredSymbol(s))?;
                    let sym = Symbol::clone(&entry);
                    Ok(Self::Symb(sym))
                }
            }
            pre::Term::Appl(head, tail) => {
                let tail: Result<_, _> = tail
                    .into_iter()
                    .map(|tm| Term::scopen(tm, syms, bnd))
                    .collect();
                Ok(Self::Appl(Term::scoper(*head, syms, bnd)?, tail?))
            }
            pre::Term::Bind(binder, arg, tm) => {
                let arg = Arg::scopen(arg, syms, bnd)?;
                bnd.with_pushed(arg.id.to_string(), |bnd| {
                    let tm = Term::scoper(*tm, syms, bnd)?;
                    match binder {
                        Binder::Lam => Ok(Self::Abst(arg, tm)),
                        Binder::Pi => Ok(Self::Prod(arg, tm)),
                    }
                })
            }
        }
    }

    /// Scope a closed term.
    ///
    /// ~~~
    /// # use kontroli::error::{Error, ScopeError};
    /// # use kontroli::pre::{self, parse::parse};
    /// # use kontroli::scope::{Symbols, Term};
    /// let syms: Symbols = vec!["A"].into_iter().collect();
    /// let tm = parse::<pre::Term>(r"\ _ : A => _.")?;
    /// assert_eq!(Term::scope(tm, &syms), Err(ScopeError::Underscore));
    /// # Ok::<_, Error>(())
    /// ~~~
    pub fn scope(tm: pre::Term, syms: &Symbols<'s>) -> Result<Self, Error> {
        Self::scopen(tm, syms, &mut Stack::new())
    }
}

impl<'s> Arg<'s> {
    fn scopen(arg: Prearg, syms: &Symbols<'s>, bnd: &mut Bound) -> Result<Self, Error> {
        let ty = arg.ty.map(|ty| Term::scoper(*ty, syms, bnd)).transpose()?;
        Ok(Self::new(arg.id, ty))
    }
}

impl<'s> Pattern<'s> {
    /// Scope an open prepattern using supplied bound variables.
    fn scopen(pat: pre::Pattern, syms: &Symbols<'s>, mvar: &Bound) -> Result<Self, Error> {
        let pre::Pattern(s, args) = pat;

        if s == "_" {
            if !args.is_empty() {
                return Err(Error::PatternArguments);
            }
            Ok(Self::Joker)
        } else if let Some(idx) = mvar.iter().position(|id| *id == *s) {
            if !args.is_empty() {
                return Err(Error::PatternArguments);
            }
            Ok(Self::MVar(idx))
        } else {
            let entry = syms.get(&s).ok_or(Error::UndeclaredSymbol(s))?;
            let sym = Symbol::clone(&entry);
            let args: Result<_, _> = args
                .into_iter()
                .map(|a| Self::scopen(a, syms, mvar))
                .collect();
            Ok(Self::Symb(sym, args?))
        }
    }
}

impl<'s> Rule<'s> {
    pub fn scope(rule: pre::Rule, syms: &Symbols<'s>) -> Result<Self, Error> {
        let mut ctxs = Stack::from(rule.ctx.clone());
        let ctx = rule.ctx;
        let pre = pre::Pattern::try_from(rule.lhs).map_err(|_| Error::NoPrepattern)?;
        let pat = Pattern::scopen(pre, syms, &ctxs)?;
        let lhs = TopPattern::try_from(pat).map_err(|_| Error::NoTopPattern)?;
        let rhs = Term::scoper(rule.rhs, syms, &mut ctxs)?;
        Ok(Self { ctx, lhs, rhs })
    }
}

impl<'s> IntroType<'s> {
    pub fn scope(it: PreIntroType, syms: &Symbols<'s>) -> Result<Self, Error> {
        let mut bnd = Stack::new();
        it.map_type_err(|tm| Term::scoper(*tm, syms, &mut bnd))?
            .map_term_err(|tm| Term::scoper(*tm, syms, &mut bnd))
    }
}

impl<'s> Command<'s> {
    pub fn scope(cmd: pre::Command, syms: &Symbols<'s>) -> Result<Self, Error> {
        match cmd {
            pre::Command::Intro(id, args, it) => {
                let it = IntroType::scope(it.parametrise(args), syms)?;
                Ok(Self::Intro(id, it))
            }
            pre::Command::Rule(prerule) => Ok(Self::Rule(Rule::scope(prerule, syms)?)),
        }
    }
}
