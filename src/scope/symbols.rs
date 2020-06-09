//! Maps from strings to (shared) symbols.

use super::Symbol;
use crate::error::SymbolsError as Error;
use core::iter::FromIterator;
use fnv::FnvHashSet;

/// Map from strings to (shared) symbols.
#[derive(Default)]
pub struct Symbols<'s>(FnvHashSet<&'s str>);

impl<'s> Symbols<'s> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get(&self, s: &str) -> Option<Symbol<'s>> {
        self.0.get(s).copied().map(Symbol::new)
    }

    pub fn insert(&mut self, s: &'s str) -> Result<Symbol<'s>, Error> {
        // `insert` returns false if the symbol is already in the set
        if !self.0.insert(s) {
            return Err(Error::Reinsertion);
        }
        Ok(Symbol::new(s))
    }
}

impl<'s> FromIterator<&'s str> for Symbols<'s> {
    fn from_iter<I: IntoIterator<Item = &'s str>>(iter: I) -> Self {
        Self(iter.into_iter().collect())
    }
}
