//! Shared strings with fast hashing and equality check.

use alloc::{rc::Rc, string::String};
use core::fmt;
use core::hash::{Hash, Hasher};

#[derive(Clone, Debug)]
pub struct Symbol(Rc<String>);

impl Symbol {
    pub fn new(s: String) -> Self {
        Self(Rc::new(s))
    }
}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        core::ptr::hash(&**self.0, state)
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Symbol {}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}
