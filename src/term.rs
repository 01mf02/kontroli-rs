pub type Arg = (Option<String>, Option<BTerm>);
pub type BTerm = Box<Term>;

pub type DeBruijn = usize;

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    Kind,
    Type,
    Symb(String),
    BVar(DeBruijn),
    Appl(BTerm, Vec<BTerm>),
    Abst(Arg, BTerm),
    Prod(Arg, BTerm),
}

use crate::reduce::{Signature, State};

impl Term {
    pub fn absts(self, args: Vec<Arg>) -> Term {
        args.into_iter()
            .rev()
            .fold(self, |acc, arg| Term::Abst(arg, Box::new(acc)))
    }
    pub fn prods(self, args: Vec<Arg>) -> Term {
        args.into_iter()
            .rev()
            .fold(self, |acc, arg| Term::Prod(arg, Box::new(acc)))
    }

    pub fn apply(mut self, mut args: Vec<BTerm>) -> Term {
        if args.is_empty() {
            self
        } else {
            match self {
                Self::Appl(_, ref mut args1) => {
                    args1.append(&mut args);
                    self
                }
                _ => Self::Appl(Box::new(self), args),
            }
        }
    }

    pub fn whnf(self, sig: &Signature) -> Self {
        Term::from(State::new(self).whnf(sig))
    }
}
