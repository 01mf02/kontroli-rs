//! Maps from strings to (shared) symbols.

use super::{symbol, Symbol};
use crate::error::SymbolsError as Error;
use alloc::{string::String, vec::Vec};
use core::borrow::Borrow;
use fnv::FnvHashMap;
use nested_modules::Context;

/// Map from strings to (shared) symbols.
#[derive(Default)]
pub struct Symbols<'s> {
    ctx: Context<String, FnvHashMap<String, &'s symbol::Owned>>,
    /// number of previously introduced symbols
    idx: usize,
}

impl<'s> Symbols<'s> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get<S: Borrow<str> + Ord>(&self, path: &[S], name: &S) -> Option<Symbol<'s>> {
        self.ctx
            .find(path.iter().map(|p| p.borrow()))
            .filter_map(|module| module.data.get(name.borrow()))
            .next()
            .copied()
            .map(Symbol::new)
    }

    pub fn get_idx(&self) -> usize {
        self.idx
    }

    pub fn insert(&mut self, name: String, s: &'s symbol::Owned) -> Result<Symbol<'s>, Error> {
        // `insert` returns false if the symbol is already in the set
        if self.ctx.get_mut().data.insert(name, s).is_some() {
            return Err(Error::Reinsertion);
        }
        self.idx += 1;
        Ok(Symbol::new(s))
    }

    pub fn set_path(&mut self, path: Vec<String>) {
        while self.ctx.close() {}
        path.into_iter().for_each(|p| self.ctx.open_or_default(p))
    }
}

/*
impl<'s> FromIterator<&'s str> for Symbols<'s> {
    fn from_iter<I: IntoIterator<Item = &'s str>>(iter: I) -> Self {
        let set: FnvHashSet<_> = iter.into_iter().collect();
        Self(Context::from(nested_modules::Module::from(set)))
    }
}
*/
