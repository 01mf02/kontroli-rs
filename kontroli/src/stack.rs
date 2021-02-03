//! A `Vec` that is iterated from the last to the first pushed element.
// TODO: rename to lifo?

use alloc::vec::Vec;
use core::iter::FromIterator;

/// A `Vec` that is iterated from the last to the first pushed element.
///
/// The motivation for this data structure is
/// the translation of algorithms using (immutable) linked lists.
/// These algorithms usually assume that
/// the nth element of a list  is the nth-*last*  consed element, whereas
/// the nth element of a `Vec` is the nth-*first* pushed element.
/// This data structure uses a `Vec` inside, but
/// the nth element of a `Stack` is the nth-*last* pushed element, that is,
/// the nth element counted from the end of the underlying `Vec`.
///
/// This behaviour is convenient e.g. when dealing with de Bruijn indices.
#[derive(Clone, Debug)]
pub struct Stack<A>(Vec<A>);

impl<A> Stack<A> {
    /// Create an empty stack.
    pub fn new() -> Self {
        Default::default()
    }

    /// Add an element to the top of the stack.
    pub fn push(&mut self, x: A) {
        self.0.push(x)
    }

    /// Remove and return an element from the top of the stack.
    pub fn pop(&mut self) -> Option<A> {
        self.0.pop()
    }

    /// Remove n elements from the top of the stack.
    pub fn pop_many(&mut self, n: usize) {
        self.0.truncate(self.len() - n)
    }

    /// Remove all elements from the stack.
    pub fn clear(&mut self) {
        self.0.clear()
    }

    /// Return the number of elements on the stack.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Return true if the stack contains no elements.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Obtain the nth element counted from the top of the stack.
    pub fn get(&self, n: usize) -> Option<&A> {
        self.iter().nth(n)
    }

    /// Push an element on the stack, run a function on it, then pop the element.
    ///
    /// This is to simulate function calls like `f(Cons(x, l))`,
    /// which assume that `l` is not changed in this call.
    /// If `with_pushed` is used consistently, then
    /// `l` at the end will contain the same elements as at the beginning.
    pub fn with_pushed<F, Y, E>(&mut self, x: A, f: F) -> Result<Y, E>
    where
        F: FnOnce(&mut Stack<A>) -> Result<Y, E>,
    {
        self.0.push(x);
        let y = f(self)?;
        self.0.pop();
        Ok(y)
    }

    /// Iterate through the elements of the stack starting from the top.
    pub fn iter(&self) -> impl Iterator<Item = &A> {
        self.0.iter().rev()
    }
}

impl<A> Default for Stack<A> {
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl<A> From<Vec<A>> for Stack<A> {
    fn from(v: Vec<A>) -> Self {
        Self(v)
    }
}

impl<A> IntoIterator for Stack<A> {
    type Item = A;
    type IntoIter = core::iter::Rev<alloc::vec::IntoIter<Self::Item>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter().rev()
    }
}

impl<A> FromIterator<A> for Stack<A> {
    fn from_iter<I: IntoIterator<Item = A>>(iter: I) -> Self {
        Self(Vec::from_iter(iter))
    }
}
