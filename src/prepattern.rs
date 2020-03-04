//! Rewrite patterns not distinguishing bound and unbound symbols.

use crate::preterm::{Binder, Preterm};

#[derive(Clone)]
pub enum Prepattern {
    Abst(Option<String>, Box<Prepattern>),
    Symb(String, Vec<Prepattern>),
}

impl From<Preterm> for Prepattern {
    fn from(tm: Preterm) -> Self {
        use Preterm::*;
        match tm {
            Appl(head, mut args) => match *head {
                Symb(s) => Self::Symb(s, args.into_iter().map(Self::from).collect()),
                Appl(head2, mut args2) => {
                    args2.append(&mut args);
                    Self::from(Appl(head2, args2))
                }
                _ => unimplemented!(),
            },
            Symb(s) => Self::Symb(s, Vec::new()),
            // TODO: warn if arg.type given?
            Bind(Binder::Lam, arg, tm) => Self::Abst(arg.id, Box::new(Self::from(*tm))),
            _ => unimplemented!(),
        }
    }
}
