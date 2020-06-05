//! Map from strings to (shared) symbols.

use super::Symbol;
use crate::error::SymbolsError as Error;
use alloc::string::String;
use core::iter::FromIterator;
use fnv::FnvHashMap;

#[derive(Default)]
pub struct Symbols<'s>(FnvHashMap<String, Symbol<'s>>);

impl<'s> Symbols<'s> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get(&self, s: &str) -> Option<&Symbol<'s>> {
        self.0.get(s)
    }

    pub fn insert(&mut self, s: &'s str) -> Result<Symbol<'s>, Error> {
        let sym = Symbol::new(s);
        if self.0.insert(String::from(s), sym).is_some() {
            return Err(Error::Reinsertion);
        }
        Ok(sym)
    }
}

impl<'s> FromIterator<&'s str> for Symbols<'s> {
    fn from_iter<I: IntoIterator<Item = &'s str>>(iter: I) -> Self {
        Self(
            iter.into_iter()
                .map(|s| (String::from(s), Symbol::new(s)))
                .collect(),
        )
    }
}
