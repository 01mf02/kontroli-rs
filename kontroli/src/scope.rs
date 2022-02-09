//! Scoping of parse structures, distinguishing variables from constants.

use crate::parse;
use crate::Arg;
use alloc::{string::String, string::ToString, vec::Vec};

use crate::parse::term::Atom;
pub use crate::parse::Symb as Symbol;

pub type Term<S> = crate::Term<Symbol<S>, BTerm<S>>;
pub type TermC<S> = crate::bterm::TermC<Symbol<S>, String>;
pub type BTerm<S> = crate::BTerm<Symbol<S>, String>;

pub type Intro<S> = crate::Intro<Term<S>>;
pub type Rule<S> = crate::Rule<Arg<String, Option<Term<S>>>, Term<S>>;
pub type Command<S> = crate::Command<String, Intro<S>, Rule<S>>;

impl<R: ToString, S: From<R>> Into<Term<S>> for parse::term::Term1<Atom<Symbol<R>>, R> {
    fn into(self) -> Term<S> {
        match self {
            Self::Atom(Atom::Const(Symbol { path, name })) => {
                Term::Symb(Symbol { path, name }.map(|s| s.into()))
            }
            Self::Atom(Atom::Var(x)) => Term::BVar(x),
            Self::Atom(Atom::Type) => Term::Type,
            Self::Prod(x, ty, tm) => {
                let id = x.map(|x| x.to_string()).unwrap_or_else(|| "$".to_string());
                let ty = (*ty).into();
                let prod = TermC::Prod(Arg { id, ty }, (*tm).into());
                Term::Comb(BTerm::new(prod))
            }
            Self::Abst(x, ty, tm) => {
                let id = x.to_string();
                let ty = ty.map(|ty| (*ty).into());
                let abst = TermC::Abst(Arg { id, ty }, (*tm).into());
                Term::Comb(BTerm::new(abst))
            }
        }
    }
}

impl<R: ToString, S: From<R>> Into<Term<S>> for parse::Term<Atom<Symbol<R>>, R> {
    fn into(self) -> Term<S> {
        let head = self.0.into();
        if self.1.is_empty() {
            head
        } else {
            let tail = self.1.into_iter().map(|tm| tm.into()).collect();
            Term::Comb(BTerm::new(TermC::Appl(head, tail)))
        }
    }
}

impl<R: ToString, S: From<R>> Into<Rule<S>> for parse::Rule<R, parse::Term<Atom<Symbol<R>>, R>> {
    fn into(self) -> Rule<S> {
        let mut ctx = Vec::new();
        for (id, ty) in self.ctx {
            let ty = ty.map(|ty| ty.into());
            let id = id.to_string();
            ctx.push(Arg { id, ty });
        }

        let lhs = self.lhs.into();
        let rhs = self.rhs.into();

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

impl<R: Clone + ToString, S: From<R>> Into<Command<S>>
    for parse::Command<R, R, parse::Term<Atom<Symbol<R>>, R>>
{
    fn into(self) -> Command<S> {
        match self {
            Self::Intro(id, args, it) => {
                use parse::term::App;
                use parse::term::Term1::{Abst, Prod};

                let id = id.to_string();
                let args = args.into_iter().rev();
                let it = args.fold(crate::Intro::from(it), |it, (name, arg_ty)| {
                    let prid = Some(name.clone());
                    it.map_type(|ty| App::new(Prod(prid, arg_ty.clone().into(), ty.into())))
                        .map_term(|tm| App::new(Abst(name, Some(arg_ty.into()), tm.into())))
                });
                Command::Intro(id, it.map_type(|tm| tm.into()).map_term(|tm| tm.into()))
            }
            Self::Rules(rules) => Command::Rules(rules.into_iter().map(|r| r.into()).collect()),
        }
    }
}

// TODO: make this `#[cfg(test)]`
// for that, rewrite example in lib.rs
impl<'s> Command<&'s str> {
    /// Parse a command and scope it. Used for testing.
    pub fn parse(i: &'s str) -> Self {
        parse::Command::parse_str(i).unwrap().into()
    }
}

impl<'s> Term<&'s str> {
    /// Parse a term and scope it. Used for testing.
    pub fn parse(i: &'s str) -> Self {
        parse::Term::parse_str(i).unwrap().into()
    }
}

impl<'s> Rule<&'s str> {
    /// Parse a rule and scope it. Used for testing.
    pub fn parse(i: &'s str) -> Self {
        match parse::Command::parse_str(i).unwrap() {
            parse::Command::Rules(mut rules) => rules.pop().unwrap().into(),
            _ => panic!("command is not a rule"),
        }
    }
}
