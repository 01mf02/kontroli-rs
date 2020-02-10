use crate::rule::Error;
use crate::signature::Signature;
use crate::symbol::Symbol;
use crate::term::{fmt_appl, DeBruijn, Term};
use std::collections::HashMap;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Miller(pub usize);

impl std::fmt::Display for Miller {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "μ{}", self.0)
    }
}

#[derive(Clone)]
pub enum Pattern {
    MVar(Miller, Vec<DeBruijn>),
    Abst(Option<String>, Box<Pattern>),
    Symb(Symbol, Vec<Pattern>),
    BVar(DeBruijn, Vec<Pattern>),
}

impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Symb(s, pats) => fmt_appl(&Term::Symb(s.clone()), pats, f),
            Self::BVar(x, pats) => fmt_appl(&Term::BVar(*x), pats, f),
            Self::MVar(m, dbs) => {
                let tail: Vec<_> = dbs.iter().map(|db| Term::BVar(*db)).collect();
                fmt_appl(m, &tail, f)
            }
            Self::Abst(arg, tm) => unimplemented!(),
        }
    }
}

pub type Arity = usize;

// Taken from:
// https://stackoverflow.com/questions/46766560/how-to-check-if-there-are-duplicates-in-a-slice/46767732#46767732
fn all_unique<T>(iter: T) -> bool
where
    T: IntoIterator,
    T::Item: Eq + std::hash::Hash,
{
    let mut uniq = std::collections::HashSet::new();
    iter.into_iter().all(move |x| uniq.insert(x))
}

impl Pattern {
    pub fn get_de_bruijn(self) -> Option<DeBruijn> {
        match self {
            Pattern::BVar(idx, args) if args.is_empty() => Some(idx),
            _ => None,
        }
    }

    pub fn get_symb_appl(self) -> Option<(Symbol, Vec<Pattern>)> {
        match self {
            Pattern::Symb(s, args) => Some((s, args)),
            _ => None,
        }
    }

    fn mvars<'a>(&'a self) -> Box<dyn Iterator<Item = (&Miller, &Vec<DeBruijn>)> + 'a> {
        match self {
            Self::MVar(mv, dbs) => Box::new(std::iter::once((mv, dbs))),
            Self::Abst(_, pat) => pat.mvars(),
            Self::Symb(_, pats) | Self::BVar(_, pats) => {
                Box::new(pats.iter().map(|p| p.mvars()).flatten())
            }
        }
    }

    pub fn arities(&self, mvars: Vec<String>) -> Result<Vec<(String, Arity)>, Error> {
        // TODO: use Vec instead of HashMap for arities
        let mut arities = HashMap::new();
        for (m, args) in self.mvars() {
            if !all_unique(args.clone()) {
                return Err(Error::MillerPattern);
            }
            if arities.insert(*m, args.len()).is_some() {
                return Err(Error::NonLinearPattern);
            }
        }
        let result: Option<Vec<_>> = mvars
            .into_iter()
            .enumerate()
            .map(|(i, x)| Some((x, *arities.get(&Miller(i))?)))
            .collect();
        result.ok_or(Error::MillerUnused)
    }

    pub fn match_term(
        &self,
        tm: Term,
        sig: &Signature,
        subst: &mut Vec<Option<Term>>,
    ) -> Option<()> {
        match self {
            Self::Symb(sp, pats) => {
                let (st, tms) = tm.whnf(sig).get_symb_appl()?;
                if *sp == st && tms.len() >= pats.len() {
                    for (pat, tm) in pats.iter().zip(tms) {
                        pat.match_term(tm, sig, subst)?;
                    }
                    Some(())
                } else {
                    None
                }
            }
            Self::MVar(x, dbs) => {
                if dbs.is_empty() {
                    // TODO: what if tm is not closed?
                    let y = subst.get_mut(x.0).expect("subst");
                    match y {
                        None => {
                            *y = Some(tm);
                            Some(())
                        }
                        Some(_) => panic!("nonlinearity"),
                    }
                } else {
                    unimplemented!()
                }
            }
            _ => unimplemented!(),
        }
    }
}
