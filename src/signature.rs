use super::*;
use crate::rule::Rule;
use std::convert::TryFrom;

pub struct SymInfo {
    stat: Staticity,
    pub typ: Term,
    pub rules: Vec<Rule>,
}

// symbol -> type
pub struct Signature(FnvHashMap<String, SymInfo>);

#[derive(Clone, Debug)]
pub enum Staticity {
    Static,
    Definable,
}

type Opacity = bool;

pub enum Entry {
    Declaration(Staticity, Term),
    Definition(Opacity, Term, Term),
}

impl From<Entry> for SymInfo {
    fn from(e: Entry) -> Self {
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
                let rules = if opaque { Vec::new() } else { unimplemented!() };
                SymInfo { stat, typ, rules }
            }
        }
    }
}

impl Entry {
    pub fn declare(sig: &Signature, st: Staticity, ty: Term) -> Result<Self, typing::Error> {
        match ty.infer(&sig, &mut Vec::new())? {
            Term::Kind | Term::Type => Ok(Entry::Declaration(st, ty)),
            _ => Err(typing::Error::SortExpected),
        }
    }

    pub fn define(
        sig: &Signature,
        opaque: bool,
        oty: Option<BTerm>,
        tm: Term,
    ) -> Result<Self, typing::Error> {
        let ty = match oty {
            None => tm.infer(&sig, &mut Vec::new())?,
            Some(ty) => {
                tm.check(&sig, &mut Vec::new(), *ty.clone())?;
                *ty
            }
        };
        match ty {
            Term::Kind => Err(typing::Error::UnexpectedKind),
            _ => Ok(Entry::Definition(opaque, ty, tm)),
        }
    }
}

impl TryFrom<(DCommand, &Signature)> for Entry {
    type Error = typing::Error;

    fn try_from((dcmd, sig): (DCommand, &Signature)) -> Result<Self, Self::Error> {
        match dcmd {
            DCommand::Declaration(ty) => Self::declare(&sig, Staticity::Static, *ty),
            DCommand::Definition(oty, otm) => match (oty, otm) {
                (Some(ty), None) => Self::declare(&sig, Staticity::Definable, *ty),
                (oty, Some(tm)) => Self::define(&sig, false, oty, *tm),
                (None, None) => panic!("both type and term are empty"),
            },
            DCommand::Theorem(ty, tm) => Self::define(&sig, true, Some(ty), *tm),
        }
    }
}

impl Signature {
    pub fn new() -> Self {
        Signature(FnvHashMap::default())
    }

    pub fn get(&self, id: &str) -> Option<&SymInfo> {
        self.0.get(id)
    }

    pub fn contains_symbol(&self, id: &str) -> bool {
        self.0.contains_key(id)
    }

    pub fn insert(&mut self, id: String, entry: Entry) -> Option<SymInfo> {
        self.0.insert(id, SymInfo::from(entry))
    }
}
