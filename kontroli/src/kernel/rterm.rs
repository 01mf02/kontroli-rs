//! Pointers to shared terms.

use super::{Rc, Term};
use crate::Arg;
use alloc::vec::Vec;
use core::fmt;

/// Pointer to a shared term.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct RTerm<'s>(Rc<Term<'s>>);

impl<'s> fmt::Display for RTerm<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl<'s> RTerm<'s> {
    /// Create a term pointer from a term.
    pub fn new(t: Term<'s>) -> Self {
        Self(Rc::new(t))
    }

    /// Apply some terms to the term.
    pub fn apply(self, mut args: Vec<RTerm<'s>>) -> Self {
        if args.is_empty() {
            self
        } else {
            match &*self {
                Term::Appl(tm, args1) => {
                    let mut args1 = args1.clone();
                    args1.append(&mut args);
                    RTerm::new(Term::Appl(tm.clone(), args1))
                }
                _ => RTerm::new(Term::Appl(self, args)),
            }
        }
    }

    /// Compare the memory addresses of two term pointers.
    pub fn ptr_eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<'s, Id> Arg<Id, Option<RTerm<'s>>> {
    /// Compare the memory addresses of the argument types.
    pub fn type_ptr_eq(&self, other: &Self) -> bool {
        match (&self.ty, &other.ty) {
            (None, None) => true,
            (Some(ty1), Some(ty2)) => RTerm::ptr_eq(&ty1, &ty2),
            _ => false,
        }
    }
}

impl<'s> core::ops::Deref for RTerm<'s> {
    type Target = Term<'s>;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
