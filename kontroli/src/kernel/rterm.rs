//! Pointers to shared terms.

use super::{Rc, Term, TermC};
use alloc::vec::Vec;
use core::fmt;

/// Pointer to a shared term.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RTerm<'s>(Rc<TermC<'s>>);

impl<'s> RTerm<'s> {
    /// Create a term pointer from a term.
    pub fn new(tm: TermC<'s>) -> Self {
        Self(Rc::new(tm))
    }

    /// Compare the memory addresses of two term pointers.
    pub fn ptr_eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<'s> Term<'s> {
    /// Apply some terms to the term.
    pub fn apply(self, mut args: Vec<Self>) -> Self {
        if args.is_empty() {
            return self;
        }
        if let Term::Comb(comb) = &self {
            if let TermC::Appl(tm, args1) = &**comb {
                let mut args1 = args1.clone();
                args1.append(&mut args);
                return Term::Comb(RTerm::new(TermC::Appl(tm.clone(), args1)));
            }
        };
        Term::Comb(RTerm::new(TermC::Appl(self, args)))
    }

    /// Compare the memory addresses of two term pointers.
    pub fn ptr_eq(&self, other: &Self) -> bool {
        match (&self, &other) {
            (Term::Kind, Term::Kind) | (Term::Type, Term::Type) => true,
            (Term::Symb(c1), Term::Symb(c2)) => c1 == c2,
            (Term::BVar(v1), Term::BVar(v2)) => v1 == v2,
            (Term::Comb(l), Term::Comb(r)) => RTerm::ptr_eq(l, r),
            _ => false,
        }
    }
}

impl<'s> core::ops::Deref for RTerm<'s> {
    type Target = TermC<'s>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'s> From<TermC<'s>> for RTerm<'s> {
    fn from(tm: TermC<'s>) -> Self {
        Self::new(tm)
    }
}

impl<'s> fmt::Display for RTerm<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (**self).fmt(f)
    }
}
