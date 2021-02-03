use alloc::vec::Vec;
use core::fmt::{self, Display};

/// Rewrite rule.
#[derive(Clone, Debug)]
pub struct Rule<V, L, R> {
    /// context (bound variables)
    pub ctx: Vec<V>,
    /// left-hand side (pattern to match with)
    pub lhs: L,
    /// right-hand side (term to replace with)
    pub rhs: R,
}

impl<V, L: Display, R: Display> Display for Rule<V, L, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ⟶ {}", self.lhs, self.rhs)
    }
}
