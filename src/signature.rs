//! Map from symbols to their types and associated rewrite rules.

use crate::command::DCommand;
use crate::pattern::TopPattern;
use crate::rule::Rule;
use crate::symbol::Symbol;
use crate::term::{RTerm, Term};
use crate::typing;
use fnv::FnvHashMap;
use std::fmt;

pub struct Signature {
    pub eta: bool,
    pub types: FnvHashMap<Symbol, RTerm>,
    pub rules: FnvHashMap<Symbol, Vec<Rule>>,
}

impl Default for Signature {
    fn default() -> Self {
        Self {
            eta: false,
            types: Default::default(),
            rules: Default::default(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Staticity {
    Static,
    Definable,
}

type Opacity = bool;

// TODO: remake Entry into struct
pub enum Entry {
    Declaration(Staticity, RTerm),
    Definition(Opacity, RTerm, RTerm),
}

#[derive(Debug)]
pub enum Error {
    Reintroduction,
    NonRewritable,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "signature error")
    }
}

impl Signature {
    fn intro_type(&mut self, sym: Symbol, typ: RTerm) -> Result<(), Error> {
        if self.types.insert(sym, typ).is_some() {
            return Err(Error::Reintroduction);
        }
        Ok(())
    }

    fn intro_rules(&mut self, sym: Symbol, rules: Vec<Rule>) -> Result<(), Error> {
        if self.rules.insert(sym, rules).is_some() {
            return Err(Error::Reintroduction);
        }
        Ok(())
    }

    pub fn add_rule(&mut self, rule: Rule) -> Result<(), Error> {
        Ok(self
            .rules
            .get_mut(&rule.lhs.symbol)
            .ok_or(Error::NonRewritable)?
            .push(rule))
    }

    pub fn insert(&mut self, sym: &Symbol, e: Entry) -> Result<(), Error> {
        match e {
            Entry::Declaration(stat, typ) => {
                self.intro_type(sym.clone(), typ)?;
                if stat == Staticity::Definable {
                    self.intro_rules(sym.clone(), Vec::new())?;
                }
                Ok(())
            }
            Entry::Definition(opaque, typ, tm) => {
                self.intro_type(sym.clone(), typ)?;
                if !opaque {
                    let rule = Rule {
                        ctx: Vec::new(),
                        lhs: TopPattern::from(Symbol::clone(sym)),
                        rhs: tm,
                    };
                    self.intro_rules(sym.clone(), vec![rule])?;
                }
                Ok(())
            }
        }
    }
}

impl Entry {
    pub fn declare(sig: &Signature, st: Staticity, ty: RTerm) -> Result<Self, typing::Error> {
        match &*ty.infer_closed(&sig)? {
            Term::Kind | Term::Type => Ok(Entry::Declaration(st, ty)),
            _ => Err(typing::Error::SortExpected),
        }
    }

    pub fn define(
        sig: &Signature,
        opaque: bool,
        oty: Option<RTerm>,
        tm: RTerm,
    ) -> Result<Self, typing::Error> {
        let ty = match oty {
            None => tm.infer_closed(&sig)?,
            Some(ty) => {
                let _ = ty.infer_closed(&sig)?;
                if tm.check_closed(&sig, ty.clone())? {
                    ty
                } else {
                    return Err(typing::Error::Unconvertible);
                }
            }
        };
        match &*ty {
            Term::Kind => Err(typing::Error::UnexpectedKind),
            _ => Ok(Entry::Definition(opaque, ty, tm)),
        }
    }

    pub fn new(dcmd: DCommand, sig: &Signature) -> Result<Self, typing::Error> {
        match dcmd {
            DCommand::Declaration(ty) => Self::declare(&sig, Staticity::Static, ty),
            DCommand::Definition(oty, otm) => match (oty, otm) {
                (Some(ty), None) => Self::declare(&sig, Staticity::Definable, ty),
                (oty, Some(tm)) => Self::define(&sig, false, oty, tm),
                (None, None) => panic!("both type and term are empty"),
            },
            DCommand::Theorem(ty, tm) => Self::define(&sig, true, Some(ty), tm),
        }
    }
}
