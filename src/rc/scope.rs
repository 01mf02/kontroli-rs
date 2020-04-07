//! Conversion from preterms to terms, from prepatterns to prepatterns etc.

use super::command::{Command, IntroType};
use super::pattern::{Miller, Pattern, TopPattern};
use super::term::{Arg, RTerm, Term};
use super::{Rule, Symbol, Symbols};
use crate::pre::precommand::{PreIntroType, Precommand};
use crate::pre::prepattern::{Prepattern, TryFromPrepatternError};
use crate::pre::preterm::{Binder, Prearg, Preterm};
use crate::pre::Prerule;
use crate::stack::Stack;
use alloc::{string::String, string::ToString};
use core::convert::TryFrom;

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    UndeclaredSymbol(String),
    Underscore,
    NoPrepattern,
    NoTopPattern,
}

impl From<TryFromPrepatternError> for Error {
    fn from(_: TryFromPrepatternError) -> Self {
        Self::NoPrepattern
    }
}

type Bound = Stack<String>;

impl RTerm {
    fn scopen(tm: Preterm, syms: &Symbols, bnd: &mut Bound) -> Result<Self, Error> {
        Ok(Self::new(Term::scopen(tm, syms, bnd)?))
    }
}

impl Term {
    /// Scope an open preterm using supplied bound variables.
    pub fn scopen(tm: Preterm, syms: &Symbols, bnd: &mut Bound) -> Result<Self, Error> {
        match tm {
            Preterm::Symb(s) => {
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
            Preterm::Appl(head, tail) => {
                let tail: Result<_, _> = tail
                    .into_iter()
                    .map(|tm| RTerm::scopen(tm, syms, bnd))
                    .collect();
                Ok(Self::Appl(RTerm::scopen(*head, syms, bnd)?, tail?))
            }
            Preterm::Bind(binder, arg, tm) => {
                let arg = Arg::scopen(arg, syms, bnd)?;
                bnd.with_pushed(arg.id.to_string(), |bnd| {
                    let tm = RTerm::scopen(*tm, syms, bnd)?;
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
    /// # use kontroli::rc::{Error, Symbols, Term};
    /// # use kontroli::rc::scope;
    /// # use kontroli::pre::Preterm;
    /// # use kontroli::pre::parse::parse;
    /// let syms: Symbols = vec!["A"].into_iter().collect();
    /// let tm = parse::<Preterm>(r"\ _ : A => _.")?;
    /// assert_eq!(Term::scope(tm, &syms), Err(scope::Error::Underscore));
    /// # Ok::<_, Error>(())
    /// ~~~
    pub fn scope(tm: Preterm, syms: &Symbols) -> Result<Self, Error> {
        Self::scopen(tm, syms, &mut Stack::new())
    }
}

impl Arg {
    fn scopen(arg: Prearg, syms: &Symbols, bnd: &mut Bound) -> Result<Self, Error> {
        let ty = arg.ty.map(|ty| RTerm::scopen(*ty, syms, bnd)).transpose()?;
        let id = Symbol::new(arg.id);
        Ok(Self { id, ty })
    }
}

impl Pattern {
    /// Scope an open prepattern using supplied bound variables.
    pub fn scopen(pat: Prepattern, syms: &Symbols, mvar: &Bound) -> Result<Self, Error> {
        let Prepattern(s, args) = pat;

        if s == "_" {
            assert!(args.is_empty());
            Ok(Self::Joker)
        } else if let Some(idx) = mvar.iter().position(|id| *id == *s) {
            assert!(args.is_empty());
            Ok(Self::MVar(Miller(idx)))
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

impl Rule {
    pub fn scope(rule: Prerule, syms: &Symbols) -> Result<Self, Error> {
        let mut ctxs = Stack::from(rule.ctx.clone());
        let ctx = rule.ctx;
        let pre = Prepattern::try_from(rule.lhs)?;
        let pat = Pattern::scopen(pre, syms, &ctxs)?;
        let lhs = TopPattern::try_from(pat)?;
        let rhs = RTerm::scopen(rule.rhs, syms, &mut ctxs)?;
        Ok(Self { ctx, lhs, rhs })
    }
}

impl IntroType {
    pub fn scope(it: PreIntroType, syms: &Symbols) -> Result<Self, Error> {
        let mut bnd = Stack::new();
        it.map_type_err(|tm| RTerm::scopen(*tm, syms, &mut bnd))?
            .map_term_err(|tm| RTerm::scopen(*tm, syms, &mut bnd))
    }
}

impl Command {
    pub fn scope(cmd: Precommand, syms: &Symbols) -> Result<Self, Error> {
        match cmd {
            Precommand::Intro(id, args, it) => {
                let it = IntroType::scope(it.parametrise(args), syms)?;
                Ok(Self::Intro(id, it))
            }
            Precommand::Rule(prerule) => Ok(Self::Rule(Rule::scope(prerule, syms)?)),
        }
    }
}
