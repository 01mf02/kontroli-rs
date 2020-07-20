//! Unshared terms, not distinguishing bound and unbound symbols.

use super::Symbol;
use alloc::{boxed::Box, string::String, string::ToString, vec::Vec};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Binder {
    Lam,
    Pi,
}

pub type BTerm = Box<Term>;

pub type Arg = crate::Arg<String, BTerm>;
pub type OptArg = crate::Arg<String, Option<BTerm>>;

impl From<Term> for Arg {
    fn from(ty: Term) -> Self {
        let id = "$".to_string();
        let ty = Box::new(ty);
        Self { id, ty }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term {
    Symb(Symbol),
    Appl(BTerm, Vec<Term>),
    Abst(OptArg, BTerm),
    Prod(Arg, BTerm),
}

impl Term {
    pub fn absts(self, args: impl Iterator<Item = Arg>) -> Self {
        args.fold(self, |acc, arg| Self::Abst(arg.map_ty(Some), Box::new(acc)))
    }

    pub fn prods(self, args: impl Iterator<Item = Arg>) -> Self {
        args.fold(self, |acc, arg| Self::Prod(arg, Box::new(acc)))
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
