use crate::error::ScopeError;
use crate::{scope, Symbol, Symbols};
use alloc::string::ToString;
use core::borrow::Borrow;

pub trait Share<'s, Target> {
    fn share(self, syms: &Symbols<'s>) -> Result<Target, ScopeError>;
}

impl<'s> Share<'s, Symbol<'s>> for Symbol<'s> {
    fn share(self, syms: &Symbols<'s>) -> Result<Symbol<'s>, ScopeError> {
        Ok(self)
    }
}

impl<'s, S: Borrow<str> + Ord> Share<'s, Symbol<'s>> for scope::Symbol<S> {
    fn share(self, syms: &Symbols<'s>) -> Result<Symbol<'s>, ScopeError> {
        syms.get(&self.path, &self.name)
            .ok_or_else(|| self.map(|s| s.borrow().to_string()).to_string())
            .map_err(ScopeError::UndeclaredSymbol)
    }
}
