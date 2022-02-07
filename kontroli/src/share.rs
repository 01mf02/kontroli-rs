use crate::error::ScopeError;
use crate::{Symbol, Symbols};
use alloc::string::ToString;
use core::borrow::Borrow;

use crate::parse::Symb;

pub trait Share<'s, Target> {
    fn share(self, syms: &Symbols<'s>) -> Result<Target, ScopeError>;
}

impl<'s> Share<'s, Symbol<'s>> for Symbol<'s> {
    fn share(self, _: &Symbols<'s>) -> Result<Symbol<'s>, ScopeError> {
        Ok(self)
    }
}

impl<'s, S: Borrow<str> + Ord> Share<'s, Symbol<'s>> for Symb<S> {
    fn share(self, syms: &Symbols<'s>) -> Result<Symbol<'s>, ScopeError> {
        syms.get(&self.path, &self.name)
            .ok_or_else(|| self.map(|s| s.borrow().to_string()).to_string())
            .map_err(ScopeError::UndeclaredSymbol)
    }
}
