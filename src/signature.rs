//! Map from symbols to their types and associated rewrite rules.

use crate::command::DCommand;
use crate::pattern::TopPattern;
use crate::rule::Rule;
use crate::symbol::Symbol;
use crate::term::{RTerm, Term};
use crate::typing;
use alloc::{vec, vec::Vec};
use core::fmt;
use fnv::FnvHashMap;

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

pub struct Entry {
    rewritable: bool,
    typ: RTerm,
    term: Option<RTerm>,
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
    pub fn new() -> Self {
        Default::default()
    }

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
        self.rules
            .get_mut(&rule.lhs.symbol)
            .ok_or(Error::NonRewritable)?
            .push(rule);
        Ok(())
    }

    pub fn insert(&mut self, sym: &Symbol, e: Entry) -> Result<(), Error> {
        self.intro_type(sym.clone(), e.typ)?;
        if e.rewritable {
            let rules = match e.term {
                None => Vec::new(),
                Some(tm) => vec![Rule {
                    ctx: Vec::new(),
                    lhs: TopPattern::from(Symbol::clone(sym)),
                    rhs: tm,
                }],
            };
            self.intro_rules(sym.clone(), rules)?;
        }
        Ok(())
    }
}

impl Entry {
    pub fn declare(sig: &Signature, rewritable: bool, typ: RTerm) -> Result<Self, typing::Error> {
        match &*typ.infer_closed(&sig)? {
            Term::Kind | Term::Type => Ok(Self {
                rewritable,
                typ,
                term: None,
            }),
            _ => Err(typing::Error::SortExpected),
        }
    }

    pub fn define(
        sig: &Signature,
        rewritable: bool,
        oty: Option<RTerm>,
        term: RTerm,
    ) -> Result<Self, typing::Error> {
        let typ = match oty {
            None => term.infer_closed(&sig)?,
            Some(ty) => {
                let _ = ty.infer_closed(&sig)?;
                if term.check_closed(&sig, ty.clone())? {
                    ty
                } else {
                    return Err(typing::Error::Unconvertible);
                }
            }
        };
        match &*typ {
            Term::Kind => Err(typing::Error::UnexpectedKind),
            _ => Ok(Self {
                rewritable,
                typ,
                term: Some(term),
            }),
        }
    }

    pub fn new(dcmd: DCommand, sig: &Signature) -> Result<Self, typing::Error> {
        match dcmd {
            DCommand::Declaration(ty) => Self::declare(&sig, false, ty),
            DCommand::Definition(oty, otm) => match (oty, otm) {
                (Some(ty), None) => Self::declare(&sig, true, ty),
                (oty, Some(tm)) => Self::define(&sig, true, oty, tm),
                (None, None) => panic!("both type and term are empty"),
            },
            DCommand::Theorem(ty, tm) => Self::define(&sig, false, Some(ty), tm),
        }
    }
}
