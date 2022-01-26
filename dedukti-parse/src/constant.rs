use crate::Consts;
use alloc::vec::Vec;
use core::borrow::Borrow;
use core::fmt::{self, Display};

/// Constant consisting of a relative module path and a symbol name.
#[derive(Clone, Debug)]
pub struct Constant<S> {
    pub path: Vec<S>,
    pub name: S,
}

impl<S> Constant<S> {
    pub fn new(path: Vec<S>, name: S) -> Self {
        Self { path, name }
    }
}

impl<S: Display> Display for Constant<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.path.iter().try_for_each(|p| write!(f, "{}.", p))?;
        self.name.fmt(f)
    }
}

pub struct Map;

impl<T: for<'a> From<&'a str>> Consts<Constant<T>> for Map {
    fn get<S: Borrow<str>>(&self, path: &[S], name: &S) -> Option<Constant<T>> {
        Some(Constant::new(
            path.iter().map(|p| p.borrow().into()).collect(),
            name.borrow().into(),
        ))
    }
}
