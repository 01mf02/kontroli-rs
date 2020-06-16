//! Unshared terms, not distinguishing bound and unbound symbols.

use alloc::{boxed::Box, string::String, string::ToString, vec::Vec};
use core::fmt;

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

pub type BTerm = Box<Term>;

pub type Arg = GArg<String, Option<BTerm>>;

impl From<Term> for Arg {
    fn from(ty: Term) -> Self {
        let id = "$".to_string();
        let ty = Some(Box::new(ty));
        Self { id, ty }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term {
    Symb(String),
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

impl<Id: fmt::Display, Ty: fmt::Display> fmt::Display for GArg<Id, Option<Ty>> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.id)?;
        if let Some(ty) = self.ty.as_ref() {
            write!(f, " : {}", ty)?;
        }
        Ok(())
    }
}
