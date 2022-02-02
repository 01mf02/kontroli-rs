//! Scoping of parse structures, distinguishing variables from constants.

use crate::parse;
use crate::{Arg, Stack};
use alloc::{string::String, string::ToString, vec::Vec};

pub use crate::parse::Symb as Symbol;

pub type Term<S> = crate::Term<Symbol<S>, BTerm<S>>;
pub type TermC<S> = crate::bterm::TermC<Symbol<S>, String>;
pub type BTerm<S> = crate::BTerm<Symbol<S>, String>;

pub type Intro<S> = crate::Intro<Term<S>>;
pub type Rule<S> = crate::Rule<Arg<String, Option<Term<S>>>, Term<S>>;
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

impl<'s, S: From<&'s str>> Scopen<'s, Term<S>> for parse::term::Term1<Symbol<&'s str>, &'s str> {
    fn scopen(self, bnd: &mut Bound<'s>) -> Term<S> {
        match self {
            Self::Const(Symbol { path, name }) => {
                if path.is_empty() {
                    if name == "Type" {
                        return Term::Type;
                    }
                    if let Some(idx) = bnd.iter().position(|id| *id == name) {
                        return Term::BVar(idx);
                    }
                }
                Term::Symb(Symbol { path, name }.map(|s| s.into()))
            }
            Self::Var(x) => Term::BVar(x),
            Self::Prod(x, ty, tm) => {
                let x = x.unwrap_or("$");
                let id = x.to_string();
                let ty = ty.scopen(bnd);
                let prod = TermC::Prod(Arg { id, ty }, bnd.with_pushed(x, |bnd| tm.scopen(bnd)));
                Term::Comb(BTerm::new(prod))
            }
            Self::Abst(x, ty, tm) => {
                let id = x.to_string();
                let ty = ty.map(|ty| ty.scopen(bnd));
                let abst = TermC::Abst(Arg { id, ty }, bnd.with_pushed(x, |bnd| tm.scopen(bnd)));
                Term::Comb(BTerm::new(abst))
            }
        }
    }
}

impl<'s, S: From<&'s str>> Scopen<'s, Term<S>> for parse::Term<Symbol<&'s str>, &'s str> {
    fn scopen(self, bnd: &mut Bound<'s>) -> Term<S> {
        let head = self.0.scopen(bnd);
        if self.1.is_empty() {
            head
        } else {
            let tail = self.1.into_iter().map(|tm| tm.scopen(bnd)).collect();
            Term::Comb(BTerm::new(TermC::Appl(head, tail)))
        }
    }
}

impl<'s, S: From<&'s str>> Scope<Rule<S>>
    for parse::Rule<&'s str, parse::Term<Symbol<&'s str>, &'s str>>
{
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

impl<'s, S: From<&'s str>> Scope<Command<S>>
    for parse::Command<&'s str, &'s str, parse::Term<Symbol<&'s str>, &'s str>>
{
    fn scope(self) -> Command<S> {
        match self {
            Self::Intro(id, args, it) => {
                use parse::term::App;
                use parse::term::Term1::{Abst, Prod};

                let id = id.to_string();
                let args = args.into_iter().rev();
                let it = args.fold(crate::Intro::from(it), |it, (name, arg_ty)| {
                    it.map_type(|ty| App::new(Prod(Some(name), arg_ty.clone().into(), ty.into())))
                        .map_term(|tm| App::new(Abst(name, Some(arg_ty.into()), tm.into())))
                });
                Command::Intro(id, it.map_type(|tm| tm.scope()).map_term(|tm| tm.scope()))
            }
            Self::Rules(rules) => Command::Rules(rules.into_iter().map(|r| r.scope()).collect()),
        }
    }
}

// TODO: make this `#[cfg(test)]`
// for that, rewrite example in lib.rs
impl<'s> Command<&'s str> {
    /// Parse a command and scope it. Used for testing.
    pub fn parse(i: &'s str) -> Self {
        parse::Command::parse_str(i).unwrap().scope()
    }
}

impl<'s> Term<&'s str> {
    /// Parse a term and scope it. Used for testing.
    pub fn parse(i: &'s str) -> Self {
        parse::Term::parse_str(i).unwrap().scope()
    }
}

impl<'s> Rule<&'s str> {
    /// Parse a rule and scope it. Used for testing.
    pub fn parse(i: &'s str) -> Self {
        match parse::Command::parse_str(i).unwrap() {
            parse::Command::Rules(mut rules) => rules.pop().unwrap().scope(),
            _ => panic!("command is not a rule"),
        }
    }
}
