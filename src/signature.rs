use super::*;

// symbol -> type
pub struct Signature(FnvHashMap<String, Term>);

#[derive(Clone, Debug)]
pub enum Staticity {
    Static,
    Definable,
}

impl Signature {
    pub fn new() -> Self {
        Signature(FnvHashMap::default())
    }

    pub fn get(&self, id: &String) -> Option<&Term> {
        self.0.get(id)
    }
}
