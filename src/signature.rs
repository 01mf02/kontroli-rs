use super::*;
use crate::rule::Rule;
use std::convert::TryFrom;
use std::rc::Rc;

pub struct SymInfo {
    stat: Staticity,
    pub symbol: Rc<String>,
    pub typ: Term,
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

// symbol -> type
#[derive(Default)]
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

impl From<(&String, Entry)> for SymInfo {
    fn from((id, e): (&String, Entry)) -> Self {
        let symbol = Rc::new(id.clone());
        match e {
            Entry::Declaration(stat, typ) => {
                let rules = Vec::new();
                SymInfo {
                    symbol,
                    stat,
                    typ,
                    rules,
                }
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
                        symbol: Rc::clone(&symbol),
                        args: Vec::new(),
                        rhs: tm,
                    }]
                };
                SymInfo {
                    symbol,
                    stat,
                    typ,
                    rules,
                }
            }
        }
    }
}

impl Entry {
    pub fn declare(sig: &Signature, st: Staticity, ty: Term) -> Result<Self, typing::Error> {
        match ty.infer(&sig, &mut typing::Context::new())? {
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
            None => tm.infer(&sig, &mut typing::Context::new())?,
            Some(ty) => {
                let _ = ty.infer(&sig, &mut typing::Context::new())?;
                tm.check(&sig, &mut typing::Context::new(), *ty.clone())?;
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
        Default::default()
    }

    pub fn get(&self, id: &str) -> Option<&SymInfo> {
        self.0.get(id)
    }

    pub fn get_mut(&mut self, id: &str) -> Option<&mut SymInfo> {
        self.0.get_mut(id)
    }

    pub fn insert(&mut self, id: String, entry: Entry) -> Option<SymInfo> {
        let info = SymInfo::from((&id, entry));
        self.0.insert(id, info)
    }
}
