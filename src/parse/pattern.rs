//! Rewrite patterns not distinguishing bound and unbound symbols.

use super::{Symbol, Term};
use alloc::vec::Vec;
use core::convert::TryFrom;

#[derive(Clone)]
pub struct Pattern(Symbol, Vec<Pattern>);

impl TryFrom<Term> for Pattern {
    type Error = ();

    fn try_from(tm: Term) -> Result<Self, Self::Error> {
        use Term::*;
        match tm {
            Appl(head, mut args) => match *head {
                Symb(s) => {
                    let args: Result<_, _> = args.into_iter().map(Self::try_from).collect();
                    Ok(Self(s, args?))
                }
                Appl(head2, mut args2) => {
                    args2.append(&mut args);
                    Self::try_from(Appl(head2, args2))
                }
                _ => Err(()),
            },
            Symb(s) => Ok(Self(s, Vec::new())),
            _ => Err(()),
        }
    }
}
