//! Map from symbols to their types and associated rewrite rules.

use crate::command::DCommand;
use crate::pattern::TopPattern;
use crate::rule::Rule;
use crate::symbol::Symbol;
use crate::term::{RTerm, Term};
use crate::typing::{Context, Error};
use fnv::FnvHashMap;

pub type Signature = FnvHashMap<Symbol, SymInfo>;

pub struct SymInfo {
    stat: Staticity,
    pub typ: RTerm,
    pub rules: Vec<Rule>,
}

impl SymInfo {
    pub fn add_rule(&mut self, rule: Rule) -> Result<(), ()> {
        match self.stat {
            Staticity::Static => Err(()),
            Staticity::Definable => {
                self.rules.push(rule);
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Staticity {
    Static,
    Definable,
}

type Opacity = bool;

pub enum Entry {
    Declaration(Staticity, RTerm),
    Definition(Opacity, RTerm, RTerm),
}

impl SymInfo {
    pub fn new(sym: &Symbol, e: Entry) -> Self {
        match e {
            Entry::Declaration(stat, typ) => {
                let rules = Vec::new();
                SymInfo { stat, typ, rules }
            }
            Entry::Definition(opaque, typ, tm) => {
                let stat = if opaque {
                    Staticity::Static
                } else {
                    Staticity::Definable
                };
                let rules = if opaque {
                    Vec::new()
                } else {
                    vec![Rule {
                        ctx: Vec::new(),
                        lhs: TopPattern::from(Symbol::clone(sym)),
                        rhs: tm,
                    }]
                };
                SymInfo { stat, typ, rules }
            }
        }
    }
}

impl Entry {
    pub fn declare(sig: &Signature, st: Staticity, ty: RTerm) -> Result<Self, Error> {
        match &*ty.infer(&sig, &mut Context::new())? {
            Term::Kind | Term::Type => Ok(Entry::Declaration(st, ty)),
            _ => Err(Error::SortExpected),
        }
    }

    pub fn define(
        sig: &Signature,
        opaque: bool,
        oty: Option<RTerm>,
        tm: RTerm,
    ) -> Result<Self, Error> {
        let ty = match oty {
            None => tm.infer(&sig, &mut Context::new())?,
            Some(ty) => {
                let _ = ty.infer(&sig, &mut Context::new())?;
                if tm.check(&sig, &mut Context::new(), ty.clone())? {
                    ty
                } else {
                    return Err(Error::Unconvertible);
                }
            }
        };
        match &*ty {
            Term::Kind => Err(Error::UnexpectedKind),
            _ => Ok(Entry::Definition(opaque, ty, tm)),
        }
    }
}

impl Entry {
    pub fn new(dcmd: DCommand, sig: &Signature) -> Result<Self, Error> {
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
