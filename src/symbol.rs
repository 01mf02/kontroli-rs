use std::fmt::{Display, Formatter, Result};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Symbol(Rc<String>);

impl Symbol {
    pub fn new(s: String) -> Self {
        Self(Rc::new(s))
    }
}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::ptr::hash(&**self.0, state)
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Symbol {}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.0.fmt(f)
    }
}
