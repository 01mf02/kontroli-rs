//! Maps from symbols to their associated types and rewrite rules.

use super::{Application, Typing};
use crate::error::SignatureError as Error;
use alloc::{string::String, vec, vec::Vec};
use core::hash::Hash;

/// Immutable HashMap for fast signature cloning.
type FnvHashMap<K, V> = im::hashmap::HashMap<K, V, fnv::FnvBuildHasher>;

type Rule<Sym, Pat, Tm> = super::Rule<String, Application<Sym, Pat>, Tm>;

/// Map from symbols to their associated types and rewrite rules.
///
/// Furthermore, set whether convertibility should be checked modulo eta.
#[derive(Clone)]
pub struct Signature<Sym, Pat, Tm> {
    pub types: FnvHashMap<Sym, Tm>,
    pub rules: FnvHashMap<Sym, Vec<Rule<Sym, Pat, Tm>>>,
    pub eta: bool,
}

impl<Sym, Pat, Tm> Default for Signature<Sym, Pat, Tm> {
    fn default() -> Self {
        Self {
            types: Default::default(),
            rules: Default::default(),
            eta: false,
        }
    }
}

impl<Sym: Clone + Eq + Hash, Pat: Clone, Tm: Clone> Signature<Sym, Pat, Tm> {
    /// Construct an empty signature without eta modularity.
    ///
    /// ~~~
    /// # use kontroli::rc::Signature;
    /// let sig = Signature::new();
    /// assert!(sig.eta == false);
    /// ~~~
    pub fn new() -> Self {
        Self {
            types: Default::default(),
            rules: Default::default(),
            eta: false,
        }
    }

    fn intro_type(&mut self, sym: Sym, typ: Tm) -> Result<(), Error> {
        if self.types.insert(sym, typ).is_some() {
            return Err(Error::Reintroduction);
        }
        Ok(())
    }

    fn intro_rules(&mut self, sym: Sym, rules: Vec<Rule<Sym, Pat, Tm>>) -> Result<(), Error> {
        if self.rules.insert(sym, rules).is_some() {
            return Err(Error::Reintroduction);
        }
        Ok(())
    }

    /// Add a rewrite rule to an existing symbol.
    pub fn add_rule(&mut self, rule: Rule<Sym, Pat, Tm>) -> Result<(), Error> {
        self.rules
            .get_mut(&rule.lhs.symbol)
            .ok_or(Error::NonRewritable)?
            .push(rule);
        Ok(())
    }

    /// Add several rewrite rules.
    pub fn add_rules<I>(&mut self, mut rules: I) -> Result<(), Error>
    where
        I: Iterator<Item = Rule<Sym, Pat, Tm>>,
    {
        rules.try_for_each(|r| self.add_rule(r))
    }

    /// Introduce a new symbol with given typing.
    pub fn insert(&mut self, sym: Sym, typing: Typing<Tm>) -> Result<(), Error> {
        self.intro_type(sym.clone(), typing.typ)?;
        if typing.rewritable {
            let rules = match typing.term {
                None => Vec::new(),
                Some((tm, _check)) => vec![Rule {
                    ctx: Vec::new(),
                    lhs: Application::from(sym.clone()),
                    rhs: tm,
                }],
            };
            self.intro_rules(sym, rules)?;
        }
        Ok(())
    }
}
