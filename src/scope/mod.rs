//! Scoping of parse structures to data structures with references.

pub mod command;
pub mod intro;
pub mod pattern;
pub mod rterm;
mod rule;
mod symbol;
mod symbols;
pub mod term;

pub use command::Command;
pub use intro::Intro;
pub use pattern::Pattern;
pub use rterm::RTerm;
pub use rule::Rule;
pub use symbol::Symbol;
pub use symbols::Symbols;
pub use term::Term;

use crate::error::{Error as KoError, ScopeError as Error};
use crate::parse::{self, parse};
use crate::stack::Stack;
use alloc::{string::String, string::ToString, vec::Vec};
use core::convert::TryFrom;
use pattern::TopPattern;
use rterm::Arg;

type Bound = Stack<String>;

impl parse::Symbol {
    fn scope<'s>(self, syms: &Symbols<'s>) -> Result<Symbol<'s>, Error> {
        syms.get(self.path, &self.name)
            .ok_or(Error::UndeclaredSymbol(self.name))
    }
}

impl parse::Term {
    fn scoper<'s>(self, syms: &Symbols<'s>, bnd: &mut Bound) -> Result<RTerm<'s>, Error> {
        Ok(RTerm::new(self.scopen(syms, bnd)?))
    }

    /// Scope an open preterm using supplied bound variables.
    fn scopen<'s>(self, syms: &Symbols<'s>, bnd: &mut Bound) -> Result<Term<'s>, Error> {
        match self {
            Self::Symb(sym) if sym.name == "_" => Err(Error::Underscore),
            Self::Symb(sym) if sym.path.is_empty() => {
                if sym.name == "Type" {
                    Ok(Term::Type)
                } else if let Some(idx) = bnd.iter().position(|id| *id == *sym.name) {
                    Ok(Term::BVar(idx))
                } else {
                    Ok(Term::Symb(sym.scope(syms)?))
                }
            }
            Self::Symb(sym) => Ok(Term::Symb(sym.scope(syms)?)),
            Self::Appl(head, tail) => {
                let tail: Result<_, _> = tail.into_iter().map(|tm| tm.scoper(syms, bnd)).collect();
                Ok(Term::Appl(head.scoper(syms, bnd)?, tail?))
            }
            Self::Bind(binder, arg, tm) => {
                let arg = arg.scopen(syms, bnd)?;
                bnd.with_pushed(arg.id.to_string(), |bnd| {
                    let tm = tm.scoper(syms, bnd)?;
                    match binder {
                        parse::term::Binder::Lam => Ok(Term::Abst(arg, tm)),
                        parse::term::Binder::Pi => Ok(Term::Prod(arg, tm)),
                    }
                })
            }
        }
    }

    /// Scope a closed term.
    ///
    /// ~~~
    /// # use kontroli::error::{Error, ScopeError};
    /// # use kontroli::parse::{self, parse};
    /// # use kontroli::scope::{Symbols, Term};
    /// let syms: Symbols = vec!["A"].into_iter().collect();
    /// let tm = parse::<parse::Term>(r"_ : A => _.\n")?.scope(&syms);
    /// assert_eq!(tm, Err(ScopeError::Underscore));
    /// # Ok::<_, Error>(())
    /// ~~~
    pub fn scope<'s>(self, syms: &Symbols<'s>) -> Result<Term<'s>, Error> {
        self.scopen(syms, &mut Stack::new())
    }
}

impl parse::term::Arg {
    fn scopen<'s>(self, syms: &Symbols<'s>, bnd: &mut Bound) -> Result<Arg<'s>, Error> {
        let ty = self.ty.map(|ty| ty.scoper(syms, bnd)).transpose()?;
        Ok(Arg { id: self.id, ty })
    }
}

impl parse::Pattern {
    /// Scope an open prepattern using supplied bound variables.
    fn scopen<'s>(self, syms: &Symbols<'s>, mvar: &Bound) -> Result<Pattern<'s>, Error> {
        let Self(s, args) = self;

        let scope = |args: Vec<Self>| -> Result<_, _> {
            args.into_iter().map(|a| a.scopen(syms, mvar)).collect()
        };

        if s.path.is_empty() {
            if s.name == "_" {
                if !args.is_empty() {
                    return Err(Error::PatternArguments);
                }
                Ok(Pattern::Joker)
            } else if let Some(idx) = mvar.iter().position(|id| *id == *s.name) {
                if !args.is_empty() {
                    return Err(Error::PatternArguments);
                }
                Ok(Pattern::MVar(idx))
            } else {
                Ok(Pattern::Symb(s.scope(syms)?, scope(args)?))
            }
        } else if s.name == "_" {
            Err(Error::Underscore)
        } else {
            Ok(Pattern::Symb(s.scope(syms)?, scope(args)?))
        }
    }
}

impl parse::Rule {
    pub fn scope<'s>(self, syms: &Symbols<'s>) -> Result<Rule<'s>, Error> {
        let mut ctxs = Stack::from(self.ctx.clone());
        let ctx = self.ctx;
        let pre = parse::Pattern::try_from(self.lhs).map_err(|_| Error::NoPrepattern)?;
        let pat = pre.scopen(syms, &ctxs)?;
        let lhs = TopPattern::try_from(pat).map_err(|_| Error::NoTopPattern)?;
        let rhs = self.rhs.scoper(syms, &mut ctxs)?;
        Ok(Rule { ctx, lhs, rhs })
    }
}

impl parse::Intro {
    pub fn scope<'s>(self, syms: &Symbols<'s>) -> Result<Intro<'s>, Error> {
        let mut bnd = Stack::new();
        self.map_type_err(|tm| tm.scoper(syms, &mut bnd))?
            .map_term_err(|tm| tm.scoper(syms, &mut bnd))
    }
}

impl parse::Command {
    pub fn scope<'s>(self, syms: &Symbols<'s>) -> Result<Command<'s, String>, Error> {
        match self {
            Self::Intro(id, args, it) => Ok(Command::Intro(id, it.parametrise(args).scope(syms)?)),
            Self::Rule(prerule) => Ok(Command::Rule(prerule.scope(syms)?)),
        }
    }
}

impl<'s> Command<'s, alloc::string::String> {
    /// Parse a command and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols<'s>) -> Result<Self, KoError> {
        Ok(parse::<parse::Command>(i)?.scope(&syms)?)
    }
}

impl<'s> Term<'s> {
    /// Parse a term and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols<'s>) -> Result<Self, KoError> {
        Ok(parse::<parse::Term>(i)?.scope(&syms)?)
    }
}

impl<'s> Rule<'s> {
    /// Parse a rule and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols<'s>) -> Result<Self, KoError> {
        Ok(parse::<parse::Rule>(i)?.scope(&syms)?)
    }
}
