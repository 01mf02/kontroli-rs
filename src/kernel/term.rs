//! Shared terms.

use super::{Rc, Symbol};
use crate::fmt::application as fmt_appl;
use crate::pre::term::GArg;
use alloc::{string::String, vec::Vec};
use core::fmt;

/// Pointer to a shared term.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct RTerm(Rc<Term>);

/// Argument of a binder.
pub type Arg = GArg<Rc<String>, Option<RTerm>>;

/// De Bruijn variable.
pub type DeBruijn = usize;

/// Shared term for the lambda-Pi calculus.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term {
    Kind,
    Type,
    Symb(Symbol),
    BVar(DeBruijn),
    Appl(RTerm, Vec<RTerm>),
    Abst(Arg, RTerm),
    Prod(Arg, RTerm),
}

impl Default for Term {
    fn default() -> Self {
        Self::Type
    }
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.id)?;
        if let Some(ty) = self.ty.as_ref() {
            write!(f, " : {}", ty)?;
        }
        Ok(())
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Kind => write!(f, "Kind"),
            Self::Type => write!(f, "Type"),
            Self::Symb(s) => write!(f, "{}", s),
            Self::BVar(x) => write!(f, "β{}", x),
            Self::Appl(head, tail) => fmt_appl(head, tail, f),
            Self::Abst(arg, tm) => write!(f, "(λ {}. {})", arg, tm),
            Self::Prod(arg, tm) => write!(f, "(Π {}. {})", arg, tm),
        }
    }
}

impl fmt::Display for RTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl RTerm {
    /// Create a term pointer from a term.
    pub fn new(t: Term) -> Self {
        Self(Rc::new(t))
    }

    /// Apply some terms to the term.
    pub fn apply(self, mut args: Vec<RTerm>) -> Self {
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

impl Arg {
    pub fn new(id: String, ty: Option<RTerm>) -> Self {
        let id = Rc::new(id);
        Self { id, ty }
    }

    /// Compare the memory addresses of the argument components.
    pub fn ptr_eq(&self, other: &Self) -> bool {
        match (self.ty.as_ref(), other.ty.as_ref()) {
            (None, None) => self.id == other.id,
            (Some(ty1), Some(ty2)) => RTerm::ptr_eq(&ty1, &ty2) && self.id == other.id,
            _ => false,
        }
    }
}

impl core::ops::Deref for RTerm {
    type Target = Term;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
