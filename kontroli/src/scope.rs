//! Scoping of parse structures, distinguishing variables from constants.

use crate::parse;
use crate::{Arg, Stack};
use alloc::{string::String, string::ToString, vec::Vec};
use core::fmt::{self, Display};

/// Symbol consisting of a relative module path and a symbol name.
#[derive(Clone, Debug)]
pub struct Symbol<S> {
    pub path: Vec<S>,
    pub name: S,
}

impl<S> Symbol<S> {
    fn new(path: Vec<S>, name: S) -> Self {
        Self { path, name }
    }

    pub fn map<T>(self, f: impl Fn(S) -> T) -> Symbol<T> {
        Symbol {
            name: f(self.name),
            path: self.path.into_iter().map(f).collect(),
        }
    }
}

impl<S: Display> Display for Symbol<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.path.iter().try_for_each(|p| write!(f, "{}.", p))?;
        self.name.fmt(f)
    }
}

pub type Term<S> = crate::Term<Symbol<S>, String, BTerm<S>>;
pub type BTerm<S> = crate::BTerm<Symbol<S>, String>;

pub type Intro<S> = crate::Intro<BTerm<S>>;
pub type Rule<S> = crate::Rule<Arg<String, Option<BTerm<S>>>, BTerm<S>>;
pub type Command<S> = crate::Command<String, Intro<S>, Rule<S>>;

type Bound<'s> = Stack<&'s str>;

pub trait Scope<Target> {
    fn scope(self) -> Target;
}

pub trait Scopen<'s, Target> {
    /// Scope an open structure using supplied bound variables.
    fn scopen(self, bnd: &mut Bound<'s>) -> Target;
}

impl<'s, Target, T: Scopen<'s, Target>> Scope<Target> for T {
    fn scope(self) -> Target {
        self.scopen(&mut Stack::new())
    }
}

impl<'s, S: From<&'s str>> Scopen<'s, BTerm<S>> for parse::Term<&'s str> {
    fn scopen(self, bnd: &mut Bound<'s>) -> BTerm<S> {
        crate::BTerm::new(self.scopen(bnd))
    }
}

impl<'s, S: From<&'s str>> Scopen<'s, Term<S>> for parse::Term<&'s str> {
    fn scopen(self, bnd: &mut Bound<'s>) -> Term<S> {
        match self {
            Self::Symb(path, name) => {
                if path.is_empty() {
                    if name == "Type" {
                        return Term::Type;
                    }
                    if let Some(idx) = bnd.iter().position(|id| *id == name) {
                        return Term::BVar(idx);
                    }
                }
                Term::Symb(Symbol::new(path, name).map(|s| s.into()))
            }
            // TODO: cover case that head is Appl?
            Self::Appl(head, tail) => {
                let tail = tail.into_iter().map(|tm| tm.scopen(bnd)).collect();
                Term::Appl(head.scopen(bnd), tail)
            }
            Self::Prod(x, ty, tm) => {
                let x = x.unwrap_or("$");
                let id = x.to_string();
                let ty = ty.scopen(bnd);
                Term::Prod(Arg { id, ty }, bnd.with_pushed(x, |bnd| tm.scopen(bnd)))
            }
            Self::Abst(x, ty, tm) => {
                let id = x.to_string();
                let ty = ty.map(|ty| ty.scopen(bnd));
                Term::Abst(Arg { id, ty }, bnd.with_pushed(x, |bnd| tm.scopen(bnd)))
            }
        }
    }
}

impl<'s, S: From<&'s str>> Scope<Rule<S>> for parse::Rule<&'s str> {
    fn scope(self) -> Rule<S> {
        let mut bnd = Bound::new();
        let mut ctx = Vec::new();
        for (id, ty) in self.ctx {
            let ty = ty.map(|ty| ty.scopen(&mut bnd));
            bnd.push(id);
            let id = id.to_string();
            ctx.push(Arg { id, ty });
        }

        let lhs = self.lhs.scopen(&mut bnd);
        let rhs = self.rhs.scopen(&mut bnd);

        Rule { ctx, lhs, rhs }
    }
}

impl<Tm> From<parse::Intro<Tm>> for crate::Intro<Tm> {
    fn from(it: parse::Intro<Tm>) -> Self {
        use parse::Intro::*;
        match it {
            Definition(ty, tm) => Self::Definition(ty, tm),
            Theorem(ty, tm) => Self::Theorem(ty, tm),
            Declaration(ty) => Self::Declaration(ty),
        }
    }
}

impl<'s, S: From<&'s str>> Scope<Command<S>> for parse::Command<&'s str> {
    fn scope(self) -> Command<S> {
        match self {
            Self::Intro(id, args, it) => {
                use alloc::boxed::Box;
                use parse::Term::{Abst, Prod};

                let id = id.to_string();
                let args = args.into_iter().rev();
                let it = args.fold(crate::Intro::from(it), |it, (name, arg_ty)| {
                    let arg_ty = Box::new(arg_ty);
                    it.map_type(|ty| Prod(Some(name), arg_ty.clone(), Box::new(ty)))
                        .map_term(|tm| Abst(name, Some(arg_ty), Box::new(tm)))
                });
                Command::Intro(id, it.map_type(|tm| tm.scope()).map_term(|tm| tm.scope()))
            }
            Self::Rules(rules) => Command::Rules(rules.into_iter().map(|r| r.scope()).collect()),
        }
    }
}

use crate::parse::{Error, Parse};

impl<'s> Command<&'s str> {
    /// Parse a command and scope it. Used for testing.
    pub fn parse(i: &'s str) -> Result<Self, Error> {
        Ok(parse::Command::parse_str(i)?.scope())
    }
}

impl<'s> Term<&'s str> {
    /// Parse a term and scope it. Used for testing.
    pub fn parse(i: &'s str) -> Result<Self, Error> {
        Ok(parse::Term::parse_str(i)?.scope())
    }
}

impl<'s> BTerm<&'s str> {
    /// Parse a term and scope it. Used for testing.
    pub fn parse(i: &'s str) -> Result<Self, Error> {
        Ok(parse::Term::parse_str(i)?.scope())
    }
}

impl<'s> Rule<&'s str> {
    /// Parse a rule and scope it. Used for testing.
    pub fn parse(i: &'s str) -> Result<Self, Error> {
        Ok(parse::Rule::parse_str(i)?.scope())
    }
}
