//! Pointers to terms.

use super::Term;
use alloc::boxed::Box;

/// Pointer to a term.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct RTerm<'s>(Box<Term<'s>>);

impl<'s> RTerm<'s> {
    pub fn new(tm: Term<'s>) -> Self {
        Self(Box::new(tm))
    }

    /// Unbox the term.
    ///
    /// It would be more elegant to implement a trait here.
    /// However, we cannot reproduce the dereferencing behaviour of `Box`,
    /// because only `Box` allows for dereferencing and moving at the same time
    /// due to being a special type.
    /// See <https://manishearth.github.io/blog/2017/01/10/rust-tidbits-box-is-special/>.
    pub fn get(self) -> Term<'s> {
        *self.0
    }
}
