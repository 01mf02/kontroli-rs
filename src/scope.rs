//! Conversion from preterms to terms, from prepatterns to prepatterns etc.

use crate::command::{Command, DCommand};
use crate::pattern::{Miller, Pattern, TopPattern};
use crate::precommand::{PreDCommand, Precommand};
use crate::prepattern::Prepattern;
use crate::prerule::Prerule;
use crate::preterm::{Binder, Prearg, Preterm};
use crate::rule::Rule;
use crate::stack::Stack;
use crate::symbol::Symbol;
use crate::symbols::Symbols;
use crate::term::{Arg, RTerm, Term};
use std::convert::TryFrom;
use std::fmt;

type Bound = Stack<String>;

pub fn bind<X, A, F>(bnd: &mut Stack<X>, arg: Option<X>, f: F) -> A
where
    F: FnOnce(&mut Stack<X>) -> A,
{
    match arg {
        Some(id) => {
            bnd.push(id);
            let x = f(bnd);
            bnd.pop();
            x
        }
        None => f(bnd),
    }
}

impl Preterm {
    fn scoper(self, syms: &Symbols, bnd: &mut Bound) -> Result<RTerm, Error> {
        Ok(RTerm::new(self.scope(syms, bnd)?))
    }

    pub fn scope(self, syms: &Symbols, bnd: &mut Bound) -> Result<Term, Error> {
        match self {
            Self::Symb(s) => {
                if s == "Type" {
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
                let arg = arg.scope(syms, bnd)?;
                bind(bnd, arg.id.clone(), |bnd| {
                    let tm = tm.scoper(syms, bnd)?;
                    match binder {
                        Binder::Lam => Ok(Term::Abst(arg, tm)),
                        Binder::Pi => Ok(Term::Prod(arg, tm)),
                    }
                })
            }
        }
    }

    pub fn scope_closed(self, syms: &Symbols) -> Result<Term, Error> {
        self.scope(syms, &mut Stack::new())
    }
}

impl Prearg {
    fn scope(self, syms: &Symbols, bnd: &mut Bound) -> Result<Arg, Error> {
        let ty = self
            .ty
            .map(|ty| Ok(RTerm::new(ty.scope(syms, bnd)?)))
            .transpose()?;
        Ok(Arg { id: self.id, ty })
    }
}

impl PreDCommand {
    pub fn scope(self, syms: &Symbols) -> Result<DCommand, Error> {
        let mut bnd = Stack::new();
        self.map_type_err(|tm| tm.scoper(syms, &mut bnd))?
            .map_term_err(|tm| tm.scoper(syms, &mut bnd))
    }
}

#[derive(Debug)]
pub enum Error {
    UndeclaredSymbol(String),
    Redeclaration,
    NoPrepattern,
    NoTopPattern,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "scoping error")
    }
}

impl Prepattern {
    pub fn scope(self, syms: &Symbols, mvar: &Bound) -> Result<Pattern, Error> {
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
            let args: Result<_, _> = args.into_iter().map(|a| a.scope(syms, mvar)).collect();
            Ok(Pattern::Symb(sym, args?))
        }
    }
}

impl Prerule {
    pub fn scope(self, syms: &Symbols) -> Result<Rule, Error> {
        let mut ctxs = Stack::from(self.ctx.clone());
        let ctx = self.ctx;
        let pre = Prepattern::try_from(self.lhs)?;
        let pat = pre.scope(syms, &ctxs)?;
        let lhs = TopPattern::try_from(pat)?;
        let rhs = self.rhs.scoper(syms, &mut ctxs)?;
        Ok(Rule { ctx, lhs, rhs })
    }
}

impl Precommand {
    pub fn scope(self, syms: &mut Symbols) -> Result<Command, Error> {
        match self {
            Self::DCmd(id, args, dcmd) => {
                let dcmd = dcmd.parametrise(args).scope(syms)?;
                let sym = Symbol::new(id.clone());
                if syms.insert(id, Symbol::clone(&sym)).is_some() {
                    return Err(Error::Redeclaration);
                };
                Ok(Command::DCmd(sym, dcmd))
            }
            Self::Rule(prerule) => Ok(Command::Rule(prerule.scope(syms)?)),
        }
    }
}
