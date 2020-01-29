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
type ArityMap = HashMap<Miller, Arity>;

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

    fn arities(&self, mvars: Vec<String>) -> Result<Vec<(String, Arity)>, Error> {
        let mut arities = HashMap::new();
        self.add_arities(&mvars, &mut arities)?;
        let result: Option<Vec<_>> = mvars
            .into_iter()
            .enumerate()
            .map(|(i, x)| Some((x, *arities.get(&Miller(i))?)))
            .collect();
        result.ok_or(Error::MillerUnused)
    }

    fn add_arities(&self, mvars: &[String], ars: &mut ArityMap) -> Result<(), Error> {
        match self {
            Self::MVar(m, args) => {
                if !all_unique(args.clone()) {
                    return Err(Error::MillerPattern);
                }

                match ars.insert(*m, args.len()) {
                    None => Ok(()),
                    Some(_) => Err(Error::NonLinearPattern),
                }
            }
            Self::Abst(arg, pat) => pat.add_arities(mvars, ars),
            Self::Symb(_, args) | Self::BVar(_, args) => {
                for a in args {
                    a.add_arities(mvars, ars)?
                }
                Ok(())
            }
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
    pub ctx: Vec<String>,
    pub pat: Pattern,
    pub rhs: Term,
}

pub struct RuleInfo {
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

impl TryFrom<Rule> for RuleInfo {
    type Error = Error;
    fn try_from(rule: Rule) -> Result<Self, Self::Error> {
        let ctx = rule.pat.arities(rule.ctx)?;
        let (symbol, args) = rule
            .pat
            .get_symb_appl()
            .ok_or(Error::AVariableIsNotAPattern)?;
        Ok(RuleInfo {
            ctx,
            symbol,
            args,
            rhs: rule.rhs,
        })
    }
}
