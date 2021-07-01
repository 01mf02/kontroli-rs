use crate::error::ScopeError;
use crate::{Symbol, Symbols};
use alloc::{string::ToString, vec::Vec};
use core::borrow::Borrow;

pub trait Share<'s, Target> {
    fn share(self, syms: &Symbols<'s>) -> Result<Target, ScopeError>;
}

impl<'s> Share<'s, Symbol<'s>> for crate::parse::Symbol {
    fn share(self, syms: &Symbols<'s>) -> Result<Symbol<'s>, ScopeError> {
        if self.name == "_" {
            Err(ScopeError::Underscore)
        } else {
            syms.get(&self.path, &self.name)
                .ok_or_else(|| ScopeError::UndeclaredSymbol(self.to_string()))
        }
    }
}

impl<'s, S: Borrow<str> + Ord> Share<'s, Symbol<'s>> for (Vec<S>, S) {
    fn share(self, syms: &Symbols<'s>) -> Result<Symbol<'s>, ScopeError> {
        let (path, name) = self;
        syms.get(&path, &name).ok_or_else(|| todo!())
    }
}
