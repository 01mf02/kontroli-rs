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
type Subst = HashMap<Miller, Term>;

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

    fn match_term(&self, tm: Term, sig: &Signature, subst: &mut Subst) -> Option<()> {
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
                    match subst.insert(*x, tm) {
                        None => Some(()),
                        Some(_) => None,
                    }
                } else {
                    unimplemented!()
                }
            }
            _ => unimplemented!(),
        }
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
    NotEnoughArguments,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "rule error")
    }
}

// TODO: name type of k "Lambdas"?

impl Arg {
    fn check_arity(&self, k: usize, arities: &[(String, Arity)]) -> bool {
        self.ty.as_ref().map_or(true, |t| t.check_arity(k, arities))
    }
}

impl Term {
    fn check_arity(&self, k: usize, arities: &[(String, Arity)]) -> bool {
        match self {
            Self::Kind | Self::Type | Self::BVar(_) | Self::Symb(_) => true,
            // TODO: can Appl(Appl(.., ..), ..) occur?
            Self::Appl(head, args) => {
                (match **head {
                    Self::BVar(n) if n >= k => args.len() >= arities.get(n - k).expect("arity").1,
                    _ => true,
                }) && args.iter().all(|a| a.check_arity(k, arities))
            }
            Self::Abst(arg, tm) | Self::Prod(arg, tm) => {
                arg.check_arity(k, arities) && tm.check_arity(k + 1, arities)
            }
        }
    }
}

impl Rule {
    pub fn new(ctx: Vec<String>, pat: Pattern, rhs: Term) -> Result<Self, Error> {
        let ctx = pat.arities(ctx)?;
        let (symbol, args) = pat.get_symb_appl().ok_or(Error::AVariableIsNotAPattern)?;
        if !rhs.check_arity(0, &ctx) {
            return Err(Error::NotEnoughArguments);
        }
        Ok(Rule {
            ctx,
            symbol,
            args,
            rhs,
        })
    }
}
