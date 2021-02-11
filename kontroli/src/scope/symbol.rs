//! Shared strings with fast copying, hashing and equality checking.

use core::fmt;
use core::hash::{Hash, Hasher};

/// Shared string with fast cloning, hashing, and equality check.
///
/// This is implemented using a reference-counted pointer.
/// Cloning, hashing, and equality checking is performed on
/// the address of the pointer, making them constant-time operations.
///
/// Note that two different symbols pointing to equivalent strings
/// are not equal, as well as their hashes:
///
/// ~~~
/// # use kontroli::scope::Symbol;
/// # use std::collections::hash_map::DefaultHasher;
/// # use std::hash::{Hash, Hasher};
/// let h1 = String::from("Hello");
/// let h2 = String::from("Hello");
/// let wl = String::from("World");
/// let s1 = Symbol::new(&h1);
/// let s2 = Symbol::new(&h2);
/// let s3 = Symbol::new(&wl);
///
/// assert_eq!(s1, s1);
/// assert_eq!(s1, s1.clone());
/// assert_ne!(s1, s2);
/// assert_ne!(s1, s3);
///
/// let hash = |s: Symbol| -> u64 {
///     let mut hasher = DefaultHasher::new();
///     s.hash(&mut hasher);
///     hasher.finish()
/// };
///
/// assert_eq!(hash(s1), hash(s1.clone()));
/// assert_ne!(hash(s1), hash(s2));
/// ~~~
///
/// To consistently assign the same symbols to equivalent strings,
/// you can use the [`Symbols`] type.
///
/// [`Symbols`]: super::Symbols

#[derive(Copy, Clone, Debug)]
pub struct Symbol<'s>(&'s str);

impl<'s> Symbol<'s> {
    pub fn new(s: &'s str) -> Self {
        Self(s)
    }
}

impl<'s> Hash for Symbol<'s> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        core::ptr::hash(self.0, state)
    }
}

impl<'s> PartialEq for Symbol<'s> {
    fn eq(&self, other: &Self) -> bool {
        core::ptr::eq(self.0, other.0)
    }
}

impl<'s> Eq for Symbol<'s> {}

impl<'s> fmt::Display for Symbol<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}
