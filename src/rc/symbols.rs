//! Map from strings to (shared) symbols.

use super::Symbol;
use alloc::string::{String, ToString};
use core::iter::FromIterator;
use fnv::FnvHashMap;

#[derive(Default)]
pub struct Symbols(FnvHashMap<String, Symbol>);

impl Symbols {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get(&self, s: &str) -> Option<&Symbol> {
        self.0.get(s)
    }

    pub fn insert(&mut self, s: String, sym: Symbol) -> Option<Symbol> {
        self.0.insert(s, sym)
    }
}

impl FromIterator<String> for Symbols {
    fn from_iter<I: IntoIterator<Item = String>>(iter: I) -> Self {
        Self(
            iter.into_iter()
                .map(|s| (s.clone(), Symbol::new(s)))
                .collect(),
        )
    }
}

impl<'a> FromIterator<&'a str> for Symbols {
    fn from_iter<I: IntoIterator<Item = &'a str>>(iter: I) -> Self {
        iter.into_iter().map(|s| s.to_string()).collect()
    }
}
