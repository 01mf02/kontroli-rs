//! Map from symbols to their types and associated rewrite rules.

use crate::command::IntroType;
use crate::pattern::TopPattern;
use crate::rule::Rule;
use crate::symbol::Symbol;
use crate::term::{RTerm, Term};
use crate::typing;
use alloc::{vec, vec::Vec};
use fnv::FnvHashMap;

pub struct Signature {
    pub types: FnvHashMap<Symbol, RTerm>,
    pub rules: FnvHashMap<Symbol, Vec<Rule>>,
    pub eta: bool,
}

impl Default for Signature {
    fn default() -> Self {
        Self {
            types: Default::default(),
            rules: Default::default(),
            eta: false,
        }
    }
}

pub struct Entry {
    typ: RTerm,
    term: Option<RTerm>,
    rewritable: bool,
}

#[derive(Debug)]
pub enum Error {
    Reintroduction,
    NonRewritable,
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
    pub fn declare(typ: RTerm, rewritable: bool, sig: &Signature) -> Result<Self, typing::Error> {
        match &*typ.infer(&sig)? {
            Term::Kind | Term::Type => Ok(Self {
                rewritable,
                typ,
                term: None,
            }),
            _ => Err(typing::Error::SortExpected),
        }
    }

    pub fn define(
        oty: Option<RTerm>,
        term: RTerm,
        rewritable: bool,
        sig: &Signature,
    ) -> Result<Self, typing::Error> {
        let typ = match oty {
            None => term.infer(&sig)?,
            Some(ty) => {
                let _ = ty.infer(&sig)?;
                if term.check(&sig, ty.clone())? {
                    ty
                } else {
                    return Err(typing::Error::Unconvertible);
                }
            }
        };
        match &*typ {
            Term::Kind => Err(typing::Error::UnexpectedKind),
            _ => Ok(Self {
                typ,
                term: Some(term),
                rewritable,
            }),
        }
    }

    pub fn new(it: IntroType, sig: &Signature) -> Result<Self, typing::Error> {
        match it {
            IntroType::Declaration(ty) => Self::declare(ty, false, &sig),
            IntroType::Definition(oty, otm) => match (oty, otm) {
                (Some(ty), None) => Self::declare(ty, true, &sig),
                (oty, Some(tm)) => Self::define(oty, tm, true, &sig),
                (None, None) => Err(typing::Error::TypeAndTermEmpty),
            },
            IntroType::Theorem(ty, tm) => Self::define(Some(ty), tm, false, &sig),
        }
    }
}
