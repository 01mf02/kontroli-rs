use super::*;

// symbol -> type
pub struct Signature(FnvHashMap<String, Term>);

#[derive(Clone, Debug)]
pub enum Staticity {
    Static,
    Definable,
}

pub enum Entry {
    Declaration(Staticity, Term),
    Definition(bool, Term, Term),
}

impl Entry {
    pub fn declare(sig: &Signature, st: Staticity, ty: Term) -> Result<Self, typing::Error> {
        match ty.clone().infer(&sig, &mut Vec::new())? {
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
            None => tm.clone().infer(&sig, &mut Vec::new())?,
            Some(ty) => {
                tm.clone().check(&sig, &mut Vec::new(), *ty.clone())?;
                *ty
            }
        };
        match ty {
            Term::Kind => Err(typing::Error::UnexpectedKind),
            _ => Ok(Entry::Definition(opaque, ty, tm)),
        }
    }
}

impl Signature {
    pub fn new() -> Self {
        Signature(FnvHashMap::default())
    }

    pub fn get(&self, id: &str) -> Option<&Term> {
        self.0.get(id)
    }

    pub fn contains_symbol(&self, id: &str) -> bool {
        self.0.contains_key(id)
    }

    pub fn insert(&mut self, id: String, entry: Entry) -> Option<Term> {
        match entry {
            Entry::Declaration(st, ty) => self.0.insert(id, ty),
            Entry::Definition(opaque, ty, tm) => self.0.insert(id, ty),
        }
    }
}
