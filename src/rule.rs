use super::*;
use std::collections::HashMap;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Miller(pub usize);

pub enum Pattern {
    MVar(Miller, Vec<DeBruijn>),
    Abst(Option<String>, Box<Pattern>),
    Symb(String, Vec<Pattern>),
    BVar(DeBruijn, Vec<Pattern>),
}

type Arity = usize;

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

    fn get_symb_appl(self) -> Option<(String, Vec<Pattern>)> {
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

    fn arities(&self, mvars: Vec<String>) -> Result<Vec<(String, Arity)>, Error> {
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
}

impl From<Term> for Pattern {
    fn from(tm: Term) -> Self {
        use Term::*;
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
            Abst(arg, tm) => Self::Abst(arg.id, Box::new(Self::from(*tm))),
            _ => unimplemented!(),
        }
    }
}

pub struct Rule {
    pub ctx: Vec<(String, Arity)>,
    pub symbol: String,
    pub args: Vec<Pattern>,
    pub rhs: Term,
}

#[derive(Debug)]
pub enum Error {
    AVariableIsNotAPattern,
    MillerPattern,
    NonLinearPattern,
    MillerUnused,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "rule error")
    }
}

impl Rule {
    pub fn new(ctx: Vec<String>, pat: Pattern, rhs: Term) -> Result<Self, Error> {
        let ctx = pat.arities(ctx)?;
        let (symbol, args) = pat.get_symb_appl().ok_or(Error::AVariableIsNotAPattern)?;
        // TODO: verify that Miller variables on RHS are applied with right arity
        Ok(Rule {
            ctx,
            symbol,
            args,
            rhs,
        })
    }
}
