use alloc::vec::Vec;
use core::fmt::{self, Display};

/// Constant consisting of a relative module path and a symbol name.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Constant<S> {
    pub path: Vec<S>,
    pub name: S,
}

impl<S> Constant<S> {
    pub fn new(name: S) -> Self {
        let path = Vec::new();
        Self { path, name }
    }

    pub fn map<T>(self, f: impl Fn(S) -> T) -> Constant<T> {
        Constant {
            path: self.path.into_iter().map(&f).collect(),
            name: f(self.name),
        }
    }

    pub fn push(&mut self, name: S) {
        self.path.push(core::mem::replace(&mut self.name, name));
    }
}

impl<S: Display> Display for Constant<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.path.iter().try_for_each(|p| write!(f, "{}.", p))?;
        self.name.fmt(f)
    }
}
