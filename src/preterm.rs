//! Unshared terms, not distinguishing bound and unbound symbols.

use alloc::{boxed::Box, string::String, vec::Vec};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Binder {
    Lam,
    Pi,
}

/// Argument of a binder.
/// For example, the `x` and `A` in the term `\ x : A => t`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GArg<Id, Ty> {
    pub id: Id,
    pub ty: Ty,
}

pub type BPreterm = Box<Preterm>;

pub type Prearg = GArg<String, Option<BPreterm>>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Preterm {
    Symb(String),
    Appl(BPreterm, Vec<Preterm>),
    Bind(Binder, Prearg, BPreterm),
}

impl Preterm {
    fn bind_many(self, binders: Vec<(Binder, Prearg)>) -> Self {
        binders.into_iter().rev().fold(self, |acc, (binder, arg)| {
            Self::Bind(binder, arg, Box::new(acc))
        })
    }

    pub fn absts(self, args: Vec<Prearg>) -> Self {
        self.bind_many(args.into_iter().map(|arg| (Binder::Lam, arg)).collect())
    }
    pub fn prods(self, args: Vec<Prearg>) -> Self {
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
