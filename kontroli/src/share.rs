//! Convert from scoped to shared structures.

use crate::error::ScopeError as Error;
use crate::lterm::{Comb, LTerm};
use crate::parse::term::{AppH, Atom};
use crate::parse::{self, Symb};
use crate::{Symbol, Symbols};
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::borrow::Borrow;

type Result<T> = core::result::Result<T, Error>;

type PTerm<S> = parse::Term<Atom<Symb<S>>, S>;

pub type Command<'s> = crate::Command<String, Intro<'s>, Rule<'s>>;

/// The way we introduce a new name.
pub type Intro<'s> = crate::Intro<LTerm<'s>>;

/// Rewrite rules with strings as bound variable identifiers,
/// a top pattern (symbol application) as left-hand side, and
/// a shared term as right-hand side.
pub type Rule<'s> = crate::Rule<(String, Option<LTerm<'s>>), TopPattern<'s>, LTerm<'s>>;

/// Pattern at the left-hand side of a rewrite rule.
pub type TopPattern<'s> = crate::pattern::TopPattern<Symbol<'s>>;

/// Rewrite pattern.
pub type Pattern<'s> = crate::Pattern<Symbol<'s>>;

pub trait Share<'s, Target> {
    fn share(self, syms: &Symbols<'s>) -> Result<Target>;
}

impl<'s, S: Borrow<str> + Ord> Share<'s, Symbol<'s>> for Symb<S> {
    fn share(self, syms: &Symbols<'s>) -> Result<Symbol<'s>> {
        syms.get(&self.path, &self.name)
            .ok_or_else(|| self.map(|s| s.borrow().to_string()).to_string())
            .map_err(Error::UndeclaredSymbol)
    }
}

impl<'s, R: Borrow<str> + Ord> Share<'s, LTerm<'s>> for AppH<Atom<Symb<R>>, R> {
    fn share(self, syms: &Symbols<'s>) -> Result<LTerm<'s>> {
        match self {
            Self::Atom(Atom::Const(symb)) => Ok(LTerm::Const(symb.share(syms)?)),
            Self::Atom(Atom::Var(v)) => Ok(LTerm::Var(v)),
            Self::Atom(Atom::Type) => Ok(LTerm::Type),
            Self::Prod(x, ty, tm) => {
                let id = x
                    .map(|x| x.borrow().to_string())
                    .unwrap_or_else(|| "$".to_string());
                let ty = ty.share(syms)?;
                Ok(LTerm::Comb(Comb::Prod(id, ty, tm.share(syms)?).into()))
            }
            Self::Abst(x, ty, tm) => {
                let id = x.borrow().to_string();
                let ty = ty.map(|ty| (*ty).share(syms)).transpose()?;
                Ok(LTerm::Comb(Comb::Abst(id, ty, tm.share(syms)?).into()))
            }
        }
    }
}

impl<'s, R: Borrow<str> + Ord> Share<'s, LTerm<'s>> for PTerm<R> {
    fn share(self, syms: &Symbols<'s>) -> Result<LTerm<'s>> {
        let head = self.0.share(syms)?;
        if self.1.is_empty() {
            Ok(head)
        } else {
            let tail: Result<_> = self.1.into_iter().map(|tm| tm.share(syms)).collect();
            Ok(LTerm::Comb(Comb::Appl(head, tail?).into()))
        }
    }
}

impl<'s> TryFrom<LTerm<'s>> for Pattern<'s> {
    type Error = Error;

    fn try_from(tm: LTerm<'s>) -> Result<Self> {
        match tm {
            LTerm::Const(c) => Ok(Self::Symb(c, Vec::new())),
            LTerm::Var(v) => Ok(Self::MVar(v)),
            LTerm::Comb(comb) => match *comb {
                Comb::Appl(head, args2) => match Self::try_from(head)? {
                    Self::Symb(s, mut args) => {
                        let args2 = args2.into_iter().map(Self::try_from);
                        args.append(&mut args2.collect::<Result<_>>()?);
                        Ok(Self::Symb(s, args))
                    }
                    // TODO: rename this to `NoPattern`
                    _ => Err(Error::NoPrepattern),
                },
                _ => Err(Error::NoPrepattern),
            },
            _ => Err(Error::NoPrepattern),
        }
    }
}

impl<'s, R: Borrow<str> + Ord> Share<'s, Rule<'s>> for parse::Rule<R, PTerm<R>> {
    fn share(self, syms: &Symbols<'s>) -> Result<Rule<'s>> {
        let joker = self.ctx.len();
        let ctx = self.ctx.into_iter().map(|(id, ty)| {
            let id = id.borrow().to_string();
            let ty = ty.map(|ty| ty.share(syms)).transpose()?;
            Ok((id, ty))
        });
        let ctx = ctx.collect::<Result<_>>()?;

        let lhs = self.lhs.share(syms)?;
        let lhs = Pattern::try_from(lhs)?;
        let lhs = lhs.joke(joker);
        let lhs = TopPattern::try_from(lhs).map_err(|_| Error::NoTopPattern)?;

        let rhs = self.rhs.share(syms)?;

        Ok(Rule { ctx, lhs, rhs })
    }
}

impl<'s, Tm: Share<'s, LTerm<'s>>> Share<'s, Intro<'s>> for parse::Intro<Tm> {
    fn share(self, syms: &Symbols<'s>) -> Result<Intro<'s>> {
        match self {
            Self::Definition(ty, tm) => Ok(Intro::Definition(
                ty.map(|ty| ty.share(syms)).transpose()?,
                tm.map(|tm| tm.share(syms)).transpose()?,
            )),
            Self::Theorem(ty, tm) => Ok(Intro::Theorem(ty.share(syms)?, tm.share(syms)?)),
            Self::Declaration(ty) => Ok(Intro::Declaration(ty.share(syms)?)),
        }
    }
}

impl<'s, R: Borrow<str> + Ord> Share<'s, Command<'s>> for parse::Command<R, R, PTerm<R>> {
    fn share(self, syms: &Symbols<'s>) -> Result<Command<'s>> {
        match self {
            Self::Intro(id, args, it) => {
                let mut args = args.into_iter().rev();
                let it = args.try_fold(it.share(syms)?, |it, (name, aty)| {
                    let id = name.borrow().to_string();
                    let aty = aty.share(syms)?;
                    Ok(it
                        .map_type(|ty| LTerm::Comb(Comb::Prod(id.clone(), aty.clone(), ty).into()))
                        .map_term(|tm| LTerm::Comb(Comb::Abst(id.clone(), Some(aty), tm).into())))
                })?;
                Ok(Command::Intro(id.borrow().to_string(), it))
            }
            Self::Rules(rules) => {
                let rules: Result<_> = rules.into_iter().map(|r| r.share(syms)).collect();
                Ok(Command::Rules(rules?))
            }
        }
    }
}

/*
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
*/
