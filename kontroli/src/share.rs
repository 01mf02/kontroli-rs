use crate::error::ScopeError;
use crate::{Symbol, Symbols};
use alloc::string::ToString;
use core::borrow::Borrow;

use crate::parse::term::{Ctx, Term1};
use crate::parse::{scope, Scope, Symb};

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

impl<'s, S: Borrow<str> + Ord, V: Borrow<S>> Scope<S, Symbol<'s>, V> for Symbols<'s> {
    fn scope(&self, symb: Symb<S>, ctx: &Ctx<Symbol<'s>, V>) -> scope::Result<Symbol<'s>, V> {
        scope::var(&symb, ctx)
            .or_else(|| self.get(&symb.path, &symb.name).map(Term1::Const))
            .ok_or_else(|| symb.map(|s| s.borrow().to_string()).to_string())
    }
}
