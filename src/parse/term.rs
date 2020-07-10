//! Unshared terms, not distinguishing bound and unbound symbols.

use super::Symbol;
use alloc::{boxed::Box, string::String, string::ToString, vec::Vec};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Binder {
    Lam,
    Pi,
}

pub type BTerm = Box<Term>;

pub type Arg = crate::Arg<String, Option<BTerm>>;

impl From<Term> for Arg {
    fn from(ty: Term) -> Self {
        let id = "$".to_string();
        let ty = Some(Box::new(ty));
        Self { id, ty }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term {
    Symb(Symbol),
    Appl(BTerm, Vec<Term>),
    Bind(Binder, Arg, BTerm),
}

impl Term {
    fn bind_many(self, binders: Vec<(Binder, Arg)>) -> Self {
        binders.into_iter().rev().fold(self, |acc, (binder, arg)| {
            Self::Bind(binder, arg, Box::new(acc))
        })
    }

    pub fn absts(self, args: Vec<Arg>) -> Self {
        self.bind_many(args.into_iter().map(|arg| (Binder::Lam, arg)).collect())
    }
    pub fn prods(self, args: Vec<Arg>) -> Self {
        self.bind_many(args.into_iter().map(|arg| (Binder::Pi, arg)).collect())
    }

    pub fn apply(mut self, mut args: Vec<Self>) -> Self {
        if args.is_empty() {
            self
        } else {
            match self {
                Self::Appl(_, ref mut args1) => {
                    args1.append(&mut args);
                    self
                }
                _ => Self::Appl(Box::new(self), args),
            }
        }
    }
}
