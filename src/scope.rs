use crate::command::{Command, DCommand, PreDCommand, Precommand};
use crate::pattern::{Miller, Pattern};
use crate::prepattern::Prepattern;
use crate::preterm::{Binder, Prearg, Preterm};
use crate::stack::Stack;
use crate::symbol::Symbol;
use crate::term::{Arg, Term};
use fnv::FnvHashMap;
use std::fmt;

pub type Symbols = FnvHashMap<String, Symbol>;

type Bound = Stack<String>;

pub fn bind<X, A, F>(bnd: &mut Stack<X>, arg: Option<X>, f: F) -> A
where
    F: FnOnce(&mut Stack<X>) -> A,
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
    pub fn scope(self, syms: &Symbols, bnd: &mut Bound) -> Result<Term, Error> {
        use Preterm::*;
        match self {
            Type => Ok(Term::Type),
            Symb(s) => match bnd.iter().position(|id| *id == *s) {
                Some(idx) => Ok(Term::BVar(idx)),
                None => {
                    let entry = syms.get(&s).ok_or(Error::UndeclaredSymbol(s))?;
                    let sym = Symbol::clone(&entry);
                    Ok(Term::Symb(sym))
                }
            },
            Appl(head, tail) => {
                let tail: Result<_, _> = tail.into_iter().map(|tm| tm.scope(syms, bnd)).collect();
                Ok(Term::Appl(Box::new(head.scope(syms, bnd)?), tail?))
            }
            Bind(binder, arg, tm) => {
                let arg = arg.scope(syms, bnd)?;
                bind(bnd, arg.id.clone(), |bnd| {
                    let tm = Box::new(tm.scope(syms, bnd)?);
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
    fn scope(self, syms: &Symbols, bnd: &mut Bound) -> Result<Arg, Error> {
        let ty = self
            .ty
            .map(|ty| Ok(Box::new(ty.scope(syms, bnd)?)))
            .transpose()?;
        Ok(Arg { id: self.id, ty })
    }
}

impl PreDCommand {
    pub fn scope(self, syms: &Symbols, bnd: &mut Bound) -> Result<DCommand, Error> {
        self.map_type_err(|tm| Ok(tm.scope(syms, bnd)?))?
            .map_term_err(|tm| Ok(tm.scope(syms, bnd)?))
    }
}

#[derive(Debug)]
pub enum Error {
    UndeclaredSymbol(String),
    MillerPattern,
    Redeclaration,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "scoping error")
    }
}

impl Prepattern {
    pub fn scope(self, syms: &Symbols, mvar: &Bound, bvar: &mut Bound) -> Result<Pattern, Error> {
        use Prepattern::*;
        match self {
            Symb(s, args) => {
                let args: Result<_, _> = args
                    .into_iter()
                    .map(|a| a.scope(syms, mvar, bvar))
                    .collect();
                match bvar.iter().position(|id| *id == *s) {
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
                            let entry = syms.get(&s).ok_or(Error::UndeclaredSymbol(s))?;
                            let sym = Symbol::clone(&entry);
                            Ok(Pattern::Symb(sym, args?))
                        }
                    },
                }
            }
            Abst(arg, pat) => bind(bvar, arg.clone(), |bvar| {
                Ok(Pattern::Abst(arg, Box::new(pat.scope(syms, mvar, bvar)?)))
            }),
        }
    }
}

impl Precommand {
    pub fn scope(self, syms: &mut Symbols) -> Result<Command, Error> {
        match self {
            Self::DCmd(id, args, dcmd) => {
                let dcmd = dcmd.parametrise(args).scope(syms, &mut Stack::new())?;
                let sym = Symbol::new(id.clone());
                if syms.insert(id, Symbol::clone(&sym)).is_some() {
                    return Err(Error::Redeclaration);
                };
                Ok(Command::DCmd(sym, dcmd))
            }
            Self::Rule(ctx, lhs, rhs) => {
                let mut ctxs = Stack::from(ctx.clone());
                let pat = Prepattern::from(*lhs).scope(syms, &ctxs, &mut Stack::new())?;
                let rhs = rhs.scope(syms, &mut ctxs)?;
                Ok(Command::Rule(ctx, pat, rhs))
            }
        }
    }
}
