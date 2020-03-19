//! Rewrite patterns à la Miller.

use crate::rule::Error;
use crate::symbol::Symbol;
use crate::term::{fmt_appl, DeBruijn, Term};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Miller(pub usize);

impl fmt::Display for Miller {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "μ{}", self.0)
    }
}

#[derive(Clone)]
pub struct MillerCtx {
    /// number of lambda-abstractions in front of variable
    pub depth: usize,
    /// arguments applied to the variable
    pub args: Vec<DeBruijn>,
    /// mapping from variables to their positions in the arguments (if present)
    pub arg_pos: Vec<Option<usize>>,
}

impl MillerCtx {
    pub fn new(depth: usize, args: Vec<DeBruijn>) -> Self {
        let mut arg_pos = vec![None; depth];
        for (i, n) in args.iter().rev().enumerate() {
            *arg_pos.get_mut(*n).unwrap() = Some(i);
        }
        Self {
            depth,
            args,
            arg_pos,
        }
    }
}

#[derive(Clone)]
pub enum Pattern {
    MVar(Miller, MillerCtx),
    Abst(Option<String>, Box<Pattern>),
    Symb(Symbol, Vec<Pattern>),
    BVar(DeBruijn, Vec<Pattern>),
    Joker,
}

#[derive(Clone)]
pub struct TopPattern {
    pub symbol: Symbol,
    pub args: Vec<Pattern>,
}

impl From<Symbol> for TopPattern {
    fn from(symbol: Symbol) -> Self {
        let args = Vec::new();
        Self { symbol, args }
    }
}

impl From<TopPattern> for Pattern {
    fn from(tp: TopPattern) -> Self {
        Self::Symb(tp.symbol, tp.args)
    }
}

pub struct NoTopPattern;

impl TryFrom<Pattern> for TopPattern {
    type Error = NoTopPattern;

    fn try_from(p: Pattern) -> Result<Self, Self::Error> {
        match p {
            Pattern::Symb(symbol, args) => Ok(TopPattern { symbol, args }),
            _ => Err(NoTopPattern),
        }
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Symb(s, pats) => fmt_appl(&Term::Symb(s.clone()), pats, f),
            Self::BVar(x, pats) => fmt_appl(&Term::BVar(*x), pats, f),
            Self::MVar(m, ctx) => {
                let tail: Vec<_> = ctx.args.iter().map(|db| Term::BVar(*db)).collect();
                fmt_appl(m, &tail, f)
            }
            Self::Abst(arg, pat) => {
                let arg = arg.clone().unwrap_or_else(|| "_".to_string());
                write!(f, "(λ {}. {})", arg, pat)
            }
            Self::Joker => write!(f, "_"),
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

    fn mvars<'a>(&'a self) -> Box<dyn Iterator<Item = (&Miller, &MillerCtx)> + 'a> {
        match self {
            Self::MVar(mv, ctx) => Box::new(std::iter::once((mv, ctx))),
            Self::Abst(_, pat) => pat.mvars(),
            Self::Symb(_, pats) | Self::BVar(_, pats) => {
                Box::new(pats.iter().map(|p| p.mvars()).flatten())
            }
            Self::Joker => Box::new(std::iter::empty()),
        }
    }

    pub fn arities(&self, mvars: Vec<String>) -> Result<Vec<(String, Arity)>, Error> {
        // TODO: use Vec instead of HashMap for arities
        let mut arities = HashMap::new();
        for (m, ctx) in self.mvars() {
            let args = &ctx.args;
            if !all_unique(args.clone()) {
                return Err(Error::MillerPattern);
            }
            match arities.insert(*m, args.len()) {
                Some(ar) if ar != args.len() => return Err(Error::NonLinearNonEqArguments),
                _ => (),
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
