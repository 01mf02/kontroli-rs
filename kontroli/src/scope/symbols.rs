//! Maps from strings to (shared) symbols.

use super::Symbol;
use crate::error::SymbolsError as Error;
use alloc::{string::String, vec::Vec};
use core::iter::FromIterator;
use fnv::FnvHashSet;
use nested_modules::{Context, Module};

/// Map from strings to (shared) symbols.
#[derive(Default)]
pub struct Symbols<'s>(Context<String, FnvHashSet<&'s str>>);

impl<'s> Symbols<'s> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get(&self, path: &[String], name: &str) -> Option<Symbol<'s>> {
        self.0
            .find(path)
            .filter_map(|module| module.data.get(name))
            .next()
            .copied()
            .map(Symbol::new)
    }

    pub fn insert(&mut self, s: &'s str) -> Result<Symbol<'s>, Error> {
        // `insert` returns false if the symbol is already in the set
        if !self.0.get_mut().data.insert(s) {
            return Err(Error::Reinsertion);
        }
        Ok(Symbol::new(s))
    }

    pub fn set_path(&mut self, path: Vec<String>) {
        while self.0.close() {}
        path.into_iter().for_each(|p| self.0.open_or_default(p))
    }
}

impl<'s> FromIterator<&'s str> for Symbols<'s> {
    fn from_iter<I: IntoIterator<Item = &'s str>>(iter: I) -> Self {
        let set: FnvHashSet<_> = iter.into_iter().collect();
        Self(Context::from(Module::from(set)))
    }
}
