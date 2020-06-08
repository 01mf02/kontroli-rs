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

impl pre::Term {
    fn scoper<'s>(self, syms: &Symbols<'s>, bnd: &mut Bound) -> Result<RTerm<'s>, Error> {
        Ok(RTerm::new(self.scopen(syms, bnd)?))
    }

    /// Scope an open preterm using supplied bound variables.
    fn scopen<'s>(self, syms: &Symbols<'s>, bnd: &mut Bound) -> Result<Term<'s>, Error> {
        match self {
            Self::Symb(s) => {
                if s == "_" {
                    Err(Error::Underscore)
                } else if s == "Type" {
                    Ok(Term::Type)
                } else if let Some(idx) = bnd.iter().position(|id| *id == *s) {
                    Ok(Term::BVar(idx))
                } else {
                    let entry = syms.get(&s).ok_or(Error::UndeclaredSymbol(s))?;
                    let sym = Symbol::clone(&entry);
                    Ok(Term::Symb(sym))
                }
            }
            Self::Appl(head, tail) => {
                let tail: Result<_, _> = tail.into_iter().map(|tm| tm.scopen(syms, bnd)).collect();
                Ok(Term::Appl(head.scoper(syms, bnd)?, tail?))
            }
            Self::Bind(binder, arg, tm) => {
                let arg = arg.scopen(syms, bnd)?;
                bnd.with_pushed(arg.id.to_string(), |bnd| {
                    let tm = tm.scoper(syms, bnd)?;
                    match binder {
                        Binder::Lam => Ok(Term::Abst(arg, tm)),
                        Binder::Pi => Ok(Term::Prod(arg, tm)),
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
    /// let tm = parse::<pre::Term>(r"\ _ : A => _.")?.scope(&syms);
    /// assert_eq!(tm, Err(ScopeError::Underscore));
    /// # Ok::<_, Error>(())
    /// ~~~
    pub fn scope<'s>(self, syms: &Symbols<'s>) -> Result<Term<'s>, Error> {
        self.scopen(syms, &mut Stack::new())
    }
}

impl Prearg {
    fn scopen<'s>(self, syms: &Symbols<'s>, bnd: &mut Bound) -> Result<Arg<'s>, Error> {
        let ty = self.ty.map(|ty| ty.scoper(syms, bnd)).transpose()?;
        Ok(Arg::new(self.id, ty))
    }
}

impl pre::Pattern {
    /// Scope an open prepattern using supplied bound variables.
    fn scopen<'s>(self, syms: &Symbols<'s>, mvar: &Bound) -> Result<Pattern<'s>, Error> {
        let Self(s, args) = self;

        if s == "_" {
            if !args.is_empty() {
                return Err(Error::PatternArguments);
            }
            Ok(Pattern::Joker)
        } else if let Some(idx) = mvar.iter().position(|id| *id == *s) {
            if !args.is_empty() {
                return Err(Error::PatternArguments);
            }
            Ok(Pattern::MVar(idx))
        } else {
            let sym = syms.get(&s).ok_or(Error::UndeclaredSymbol(s))?;
            let args: Result<_, _> = args.into_iter().map(|a| a.scopen(syms, mvar)).collect();
            Ok(Pattern::Symb(sym, args?))
        }
    }
}

impl pre::Rule {
    pub fn scope<'s>(self, syms: &Symbols<'s>) -> Result<Rule<'s>, Error> {
        let mut ctxs = Stack::from(self.ctx.clone());
        let ctx = self.ctx;
        let pre = pre::Pattern::try_from(self.lhs).map_err(|_| Error::NoPrepattern)?;
        let pat = pre.scopen(syms, &ctxs)?;
        let lhs = TopPattern::try_from(pat).map_err(|_| Error::NoTopPattern)?;
        let rhs = self.rhs.scoper(syms, &mut ctxs)?;
        Ok(Rule { ctx, lhs, rhs })
    }
}

impl PreIntroType {
    pub fn scope<'s>(self, syms: &Symbols<'s>) -> Result<IntroType<'s>, Error> {
        let mut bnd = Stack::new();
        self.map_type_err(|tm| tm.scoper(syms, &mut bnd))?
            .map_term_err(|tm| tm.scoper(syms, &mut bnd))
    }
}

impl pre::Command {
    pub fn scope<'s>(self, syms: &Symbols<'s>) -> Result<Command<'s>, Error> {
        match self {
            Self::Intro(id, args, it) => Ok(Command::Intro(id, it.parametrise(args).scope(syms)?)),
            Self::Rule(prerule) => Ok(Command::Rule(prerule.scope(syms)?)),
        }
    }
}
