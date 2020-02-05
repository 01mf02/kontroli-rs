#[derive(Clone, Debug)]
pub struct Stack<A>(Vec<A>);

impl<A> Stack<A> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn push(&mut self, x: A) {
        self.0.push(x)
    }

    pub fn pop(&mut self) -> Option<A> {
        self.0.pop()
    }

    pub fn pop_many(&mut self, n: usize) {
        self.0.truncate(self.len() - n)
    }

    pub fn clear(&mut self) {
        self.0.clear()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn get(&self, n: usize) -> Option<&A> {
        Some(self.0.iter().rev().nth(n)?)
    }

    pub fn with_pushed<F, Y, E>(&mut self, x: A, f: F) -> Result<Y, E>
    where
        F: FnOnce(&mut Stack<A>) -> Result<Y, E>,
    {
        self.0.push(x);
        let y = f(self)?;
        self.0.pop();
        Ok(y)
    }

    pub fn iter(&self) -> impl Iterator<Item = &A> {
        self.0.iter().rev()
    }
}

impl<A> Default for Stack<A> {
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl<A> IntoIterator for Stack<A> {
    type Item = A;
    type IntoIter = std::iter::Rev<std::vec::IntoIter<Self::Item>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter().rev()
    }
}
