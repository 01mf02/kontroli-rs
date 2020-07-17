//! Symbols consisting of a relative module path and a symbol name.

use alloc::{string::String, string::ToString, vec::Vec};
use core::fmt::{self, Display};

/// Symbol consisting of a relative module path and a symbol name.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Symbol {
    pub path: Vec<String>,
    pub name: String,
}

impl From<&str> for Symbol {
    fn from(name: &str) -> Self {
        Self {
            name: name.to_string(),
            path: Vec::new(),
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.path.iter().try_for_each(|p| write!(f, "{}.", p))?;
        self.name.fmt(f)
    }
}
