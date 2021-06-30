//! Scoping of parse structures, distinguishing variables from constants.

use crate::parse;
use crate::Stack;
use alloc::{string::String, string::ToString, vec::Vec};

pub type Term<S> = crate::Term<S, String, BTerm<S>>;
pub type BTerm<S> = crate::BTerm<S, String>;

pub type Intro<S> = crate::Intro<BTerm<S>>;
pub type Rule<S> = crate::Rule<String, BTerm<S>>;
pub type Command<S> = crate::Command<String, Intro<S>, Rule<S>>;

type Bound = Stack<String>;

pub trait Scope<Target> {
    fn scope(self) -> Target;
}

pub trait Scopen<Target> {
    /// Scope an open structure using supplied bound variables.
    fn scopen(self, bnd: &mut Bound) -> Target;
}

impl<Target, T: Scopen<Target>> Scope<Target> for T {
    fn scope(self) -> Target {
        self.scopen(&mut Stack::new())
    }
}

impl Scopen<BTerm<parse::Symbol>> for parse::Term {
    fn scopen(self, bnd: &mut Bound) -> BTerm<parse::Symbol> {
        crate::BTerm::new(self.scopen(bnd))
    }
}

impl Scopen<Term<parse::Symbol>> for parse::Term {
    fn scopen(self, bnd: &mut Bound) -> Term<parse::Symbol> {
        match self {
            Self::Symb(sym) if sym.path.is_empty() => {
                if sym.name == "Type" {
                    Term::Type
                } else if let Some(idx) = bnd.iter().position(|id| *id == *sym.name) {
                    Term::BVar(idx)
                } else {
                    Term::Symb(sym)
                }
            }
            Self::Symb(sym) => Term::Symb(sym),
            Self::Appl(head, tail) => {
                let tail = tail.into_iter().map(|tm| tm.scopen(bnd)).collect();
                Term::Appl(head.scopen(bnd), tail)
            }
            Self::Prod(arg, tm) => {
                let arg = arg.map_ty(|ty| ty.scopen(bnd));
                bnd.with_pushed(arg.id.to_string(), |bnd| Term::Prod(arg, tm.scopen(bnd)))
            }
            Self::Abst(arg, tm) => {
                let arg = arg.map_ty(|o| o.map(|ty| ty.scopen(bnd)));
                bnd.with_pushed(arg.id.to_string(), |bnd| Term::Abst(arg, tm.scopen(bnd)))
            }
        }
    }
}

impl Scope<Rule<parse::Symbol>> for parse::Rule {
    fn scope(self) -> Rule<parse::Symbol> {
        let ctx: Vec<_> = self.ctx.into_iter().map(|arg| arg.id).collect();

        let mut bnd: Bound = Stack::from(ctx.clone());
        let lhs = self.lhs.scopen(&mut bnd);
        let rhs = self.rhs.scopen(&mut bnd);

        Rule { ctx, lhs, rhs }
    }
}

impl Scope<Intro<parse::Symbol>> for parse::Intro {
    fn scope(self) -> Intro<parse::Symbol> {
        self.map_type(|tm| tm.scope()).map_term(|tm| tm.scope())
    }
}

impl Scope<Command<parse::Symbol>> for parse::Command {
    fn scope(self) -> Command<parse::Symbol> {
        match self {
            Self::Intro(id, it) => Command::Intro(id, it.scope()),
            Self::Rules(rules) => Command::Rules(rules.into_iter().map(|r| r.scope()).collect()),
        }
    }
}

use crate::{parse::parse, Error};

impl Command<parse::Symbol> {
    /// Parse a command and scope it. Used for testing.
    pub fn parse(i: &str) -> Result<Self, Error> {
        Ok(parse::<parse::Command>(i)?.scope())
    }
}

impl Term<parse::Symbol> {
    /// Parse a term and scope it. Used for testing.
    pub fn parse(i: &str) -> Result<Self, Error> {
        Ok(parse::<parse::Term>(i)?.scope())
    }
}

impl BTerm<parse::Symbol> {
    /// Parse a term and scope it. Used for testing.
    pub fn parse(i: &str) -> Result<Self, Error> {
        Ok(BTerm::new(parse::<parse::Term>(i)?.scope()))
    }
}

impl Rule<parse::Symbol> {
    /// Parse a rule and scope it. Used for testing.
    pub fn parse(i: &str) -> Result<Self, Error> {
        Ok(parse::<parse::Rule>(i)?.scope())
    }
}
