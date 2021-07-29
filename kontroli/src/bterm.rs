//! Pointers to terms.

use alloc::boxed::Box;
use core::fmt::{self, Display};

pub type TermC<C, V> = crate::term::TermC<V, crate::term::Term<C, BTerm<C, V>>>;

/// Pointer to a term.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BTerm<C, V>(Box<TermC<C, V>>);

impl<C, V> BTerm<C, V> {
    pub fn new(tm: TermC<C, V>) -> Self {
        Self(Box::new(tm))
    }

    /// Unbox the term.
    ///
    /// It would be more elegant to implement a trait here.
    /// However, we cannot reproduce the dereferencing behaviour of `Box`,
    /// because only `Box` allows for dereferencing and moving at the same time
    /// due to being a special type.
    /// See <https://manishearth.github.io/blog/2017/01/10/rust-tidbits-box-is-special/>.
    pub fn get(self) -> TermC<C, V> {
        *self.0
    }
}

impl<C: Display, V: Display> Display for BTerm<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}
