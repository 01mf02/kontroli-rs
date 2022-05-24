//! Maps from strings to (shared) symbols.

use super::{symbol, Symbol};
use crate::error::SymbolsError as Error;
use alloc::{string::String, vec::Vec};
use core::borrow::Borrow;
use fnv::FnvHashMap;
use nested_modules::Context;

/// Map from strings to (shared) symbols.
#[derive(Default)]
pub struct Symbols<'s>(Context<String, FnvHashMap<String, &'s symbol::Owned>>);

impl<'s> Symbols<'s> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get<S: Borrow<str> + Ord>(&self, path: &[S], name: &S) -> Option<Symbol<'s>> {
        self.0
            .find(path.iter().map(|p| p.borrow()))
            .filter_map(|module| module.data.get(name.borrow()))
            .next()
            .copied()
            .map(Symbol::new)
    }

    pub fn insert(&mut self, name: String, s: &'s symbol::Owned) -> Result<Symbol<'s>, Error> {
        // `insert` returns false if the symbol is already in the set
        if self.0.get_mut().data.insert(name, s).is_some() {
            return Err(Error::Reinsertion);
        }
        Ok(Symbol::new(s))
    }

    pub fn set_path(&mut self, path: Vec<String>) {
        while self.0.close() {}
        path.into_iter().for_each(|p| self.0.open_or_default(p))
    }
}
