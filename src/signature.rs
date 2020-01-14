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
}

impl Signature {
    pub fn new() -> Self {
        Signature(FnvHashMap::default())
    }

    pub fn get(&self, id: &String) -> Option<&Term> {
        self.0.get(id)
    }

    pub fn add(&mut self, id: String, entry: Entry) {
        unimplemented!()
    }
}
