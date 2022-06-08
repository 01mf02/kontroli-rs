//! Symbols consisting of a relative module path and a name.

use alloc::vec::Vec;
use core::fmt::{self, Display};

/// Symbol consisting of a relative module path and a name.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Symb<S> {
    /// module path (`["a", "b"]` for symbol `"a.b.c"`)
    pub path: Vec<S>,
    /// symbol name (`"c"` for symbol `"a.b.c"`)
    pub name: S,
}

impl<S> Symb<S> {
    /// Create a new symbol with an empty module path.
    pub fn new(name: S) -> Self {
        let path = Vec::new();
        Self { path, name }
    }

    /// Map over all parts of the symbol.
    pub fn map<T>(self, f: impl Fn(S) -> T) -> Symb<T> {
        Symb {
            path: self.path.into_iter().map(&f).collect(),
            name: f(self.name),
        }
    }

    /// Replace the symbol name with the given name, and
    /// move the previous symbol name at the end of the module path.
    pub fn push(&mut self, name: S) {
        self.path.push(core::mem::replace(&mut self.name, name));
    }
}

impl<S: Display> Display for Symb<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.path.iter().try_for_each(|p| write!(f, "{}.", p))?;
        self.name.fmt(f)
    }
}
