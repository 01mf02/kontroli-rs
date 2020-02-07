use crate::command::{DCommand, PreDCommand};
use crate::pattern::{Miller, Pattern};
use crate::prepattern::Prepattern;
use crate::preterm::{Binder, Prearg, Preterm};
use crate::signature::Signature;
use crate::term::{Arg, Term};
use std::rc::Rc;

type Bound = Vec<String>;

pub fn bind<X, A, F>(bnd: &mut Vec<X>, arg: Option<X>, f: F) -> A
where
    F: FnOnce(&mut Vec<X>) -> A,
{
    match arg {
        Some(id) => {
            bnd.push(id);
            let x = f(bnd);
            bnd.pop();
            x
        }
        None => f(bnd),
    }
}

impl Preterm {
    pub fn scope(self, sig: &Signature, bnd: &mut Bound) -> Result<Term, Error> {
        use Preterm::*;
        match self {
            Type => Ok(Term::Type),
            Symb(s) => match bnd.iter().rev().position(|id| *id == *s) {
                Some(idx) => Ok(Term::BVar(idx)),
                None => {
                    let entry = sig.get(&s).ok_or(Error::UndeclaredSymbol)?;
                    let sym = Rc::clone(&entry.symbol);
                    Ok(Term::Symb(sym))
                }
            },
            Appl(head, tail) => {
                let tail: Result<_, _> = tail.into_iter().map(|tm| tm.scope(sig, bnd)).collect();
                Ok(Term::Appl(Box::new(head.scope(sig, bnd)?), tail?))
            }
            Bind(binder, arg, tm) => {
                let arg = arg.scope(sig, bnd)?;
                bind(bnd, arg.id.clone(), |bnd| {
                    let tm = Box::new(tm.scope(sig, bnd)?);
                    match binder {
                        Binder::Lam => Ok(Term::Abst(arg, tm)),
                        Binder::Pi => Ok(Term::Prod(arg, tm)),
                    }
                })
            }
        }
    }
}

impl Prearg {
    fn scope(self, sig: &Signature, bnd: &mut Bound) -> Result<Arg, Error> {
        let ty = self
            .ty
            .map(|ty| Ok(Box::new(ty.scope(sig, bnd)?)))
            .transpose()?;
        Ok(Arg { id: self.id, ty })
    }
}

impl PreDCommand {
    pub fn scope(self, sig: &Signature, bnd: &mut Bound) -> Result<DCommand, Error> {
        self.map_type_err(|tm| Ok(Box::new(tm.scope(sig, bnd)?)))?
            .map_term_err(|tm| Ok(Box::new(tm.scope(sig, bnd)?)))
    }
}

#[derive(Debug)]
pub enum Error {
    UndeclaredSymbol,
    MillerPattern,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "scoping error")
    }
}

impl Prepattern {
    pub fn scope(self, sig: &Signature, mvar: &Bound, bvar: &mut Bound) -> Result<Pattern, Error> {
        use Prepattern::*;
        match self {
            Symb(s, args) => {
                let args: Result<_, _> =
                    args.into_iter().map(|a| a.scope(sig, mvar, bvar)).collect();
                // TODO: move bvar to Stack
                match bvar.iter().rev().position(|id| *id == *s) {
                    Some(idx) => Ok(Pattern::BVar(idx, args?)),
                    None => match mvar.iter().position(|id| *id == *s) {
                        Some(idx) => {
                            let args: Result<_, _> = args?
                                .into_iter()
                                .map(|a| Ok(a.get_de_bruijn().ok_or(Error::MillerPattern)?))
                                .collect();
                            Ok(Pattern::MVar(Miller(idx), args?))
                        }
                        None => {
                            let entry = sig.get(&s).ok_or(Error::UndeclaredSymbol)?;
                            let sym = Rc::clone(&entry.symbol);
                            Ok(Pattern::Symb(sym, args?))
                        }
                    },
                }
            }
            Abst(arg, pat) => bind(bvar, arg.clone(), |bvar| {
                Ok(Pattern::Abst(arg, Box::new(pat.scope(sig, mvar, bvar)?)))
            }),
        }
    }
}
