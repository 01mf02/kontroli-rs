//! Shared strings with fast cloning, hashing and equality checking.

use alloc::{rc::Rc, string::String};
use core::fmt;
use core::hash::{Hash, Hasher};

/// Shared string with fast cloning, hashing, and equality check.
///
/// This is implemented using a reference-counted pointer.
/// Cloning, hashing, and equality checking is performed on
/// the address of the pointer, making them constant-time operations.
///
/// Note that two different symbols pointing to equivalent strings are not equal:
///
/// ~~~
/// # use kontroli::Symbol;
/// let s1 = Symbol::new("Hello".to_string());
/// let s2 = Symbol::new("Hello".to_string());
/// let s3 = Symbol::new("World".to_string());
/// assert_eq!(s1, s1);
/// assert_ne!(s1, s2);
/// assert_ne!(s1, s3);
/// ~~~
///
/// To consistently assign the same symbols to equivalent strings,
/// you can use the [`Symbols`] type.
///
/// [`Symbols`]: ../symbols/struct.Symbols.html
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
