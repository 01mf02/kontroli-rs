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

type Bound = Stack<String>;

impl Preterm {
    fn scoper(self, syms: &Symbols, bnd: &mut Bound) -> Result<RTerm, Error> {
        Ok(RTerm::new(self.scopen(syms, bnd)?))
    }

    /// Scope an open preterm using supplied bound variables.
    pub fn scopen(self, syms: &Symbols, bnd: &mut Bound) -> Result<Term, Error> {
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
                let tail: Result<_, _> = tail.into_iter().map(|tm| tm.scoper(syms, bnd)).collect();
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
    /// # use kontroli::rc::{Error, Symbols};
    /// # use kontroli::rc::scope;
    /// # use kontroli::pre::Preterm;
    /// # use kontroli::pre::parse::parse;
    /// let syms: Symbols = vec!["A"].into_iter().collect();
    /// let tm = parse::<Preterm>(r"\ _ : A => _.")?;
    /// assert_eq!(tm.scope(&syms), Err(scope::Error::Underscore));
    /// # Ok::<_, Error>(())
    /// ~~~
    pub fn scope(self, syms: &Symbols) -> Result<Term, Error> {
        self.scopen(syms, &mut Stack::new())
    }
}

impl Prearg {
    fn scopen(self, syms: &Symbols, bnd: &mut Bound) -> Result<Arg, Error> {
        let ty = self
            .ty
            .map(|ty| Ok::<_, Error>(RTerm::new(ty.scopen(syms, bnd)?)))
            .transpose()?;
        let id = Symbol::new(self.id);
        Ok(Arg { id, ty })
    }
}

impl PreIntroType {
    pub fn scope(self, syms: &Symbols) -> Result<IntroType, Error> {
        let mut bnd = Stack::new();
        self.map_type_err(|tm| tm.scoper(syms, &mut bnd))?
            .map_term_err(|tm| tm.scoper(syms, &mut bnd))
    }
}

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

impl Prepattern {
    /// Scope an open prepattern using supplied bound variables.
    pub fn scopen(self, syms: &Symbols, mvar: &Bound) -> Result<Pattern, Error> {
        let Self(s, args) = self;

        if s == "_" {
            assert!(args.is_empty());
            Ok(Pattern::Joker)
        } else if let Some(idx) = mvar.iter().position(|id| *id == *s) {
            assert!(args.is_empty());
            Ok(Pattern::MVar(Miller(idx)))
        } else {
            let entry = syms.get(&s).ok_or(Error::UndeclaredSymbol(s))?;
            let sym = Symbol::clone(&entry);
            let args: Result<_, _> = args.into_iter().map(|a| a.scopen(syms, mvar)).collect();
            Ok(Pattern::Symb(sym, args?))
        }
    }
}

impl Prerule {
    pub fn scope(self, syms: &Symbols) -> Result<Rule, Error> {
        let mut ctxs = Stack::from(self.ctx.clone());
        let ctx = self.ctx;
        let pre = Prepattern::try_from(self.lhs)?;
        let pat = pre.scopen(syms, &ctxs)?;
        let lhs = TopPattern::try_from(pat)?;
        let rhs = self.rhs.scoper(syms, &mut ctxs)?;
        Ok(Rule { ctx, lhs, rhs })
    }
}

impl Precommand {
    pub fn scope(self, syms: &Symbols) -> Result<Command, Error> {
        match self {
            Self::Intro(id, args, it) => Ok(Command::Intro(id, it.parametrise(args).scope(syms)?)),
            Self::Rule(prerule) => Ok(Command::Rule(prerule.scope(syms)?)),
        }
    }
}
