//! Maps from symbols to their types and associated rewrite rules.

use super::pattern::TopPattern;
use super::{RTerm, Rule, Symbol, Typing};
use crate::error::SignatureError as Error;
use alloc::{vec, vec::Vec};

/// Immutable HashMap for fast signature cloning.
type FnvHashMap<K, V> = im::hashmap::HashMap<K, V, fnv::FnvBuildHasher>;

/// Map from symbols to their types and associated rewrite rules.
///
/// Furthermore, set whether convertibility should be checked modulo eta.
#[derive(Clone, Default)]
pub struct Signature<'s> {
    pub types: FnvHashMap<Symbol<'s>, RTerm<'s>>,
    pub rules: FnvHashMap<Symbol<'s>, Vec<Rule<'s>>>,
    pub eta: bool,
}

impl<'s> Signature<'s> {
    /// Construct an empty signature without eta modularity.
    ///
    /// ~~~
    /// # use kontroli::rc::Signature;
    /// let sig = Signature::new();
    /// assert!(sig.eta == false);
    /// ~~~
    pub fn new() -> Self {
        Default::default()
    }

    fn intro_type(&mut self, sym: Symbol<'s>, typ: RTerm<'s>) -> Result<(), Error> {
        if self.types.insert(sym, typ).is_some() {
            return Err(Error::Reintroduction);
        }
        Ok(())
    }

    fn intro_rules(&mut self, sym: Symbol<'s>, rules: Vec<Rule<'s>>) -> Result<(), Error> {
        if self.rules.insert(sym, rules).is_some() {
            return Err(Error::Reintroduction);
        }
        Ok(())
    }

    /// Add a rewrite rule to an existing symbol.
    pub fn add_rule(&mut self, rule: Rule<'s>) -> Result<(), Error> {
        self.rules
            .get_mut(&rule.lhs.symbol)
            .ok_or(Error::NonRewritable)?
            .push(rule);
        Ok(())
    }

    /// Introduce a new symbol with given typing.
    pub fn insert(&mut self, sym: Symbol<'s>, typing: Typing<'s>) -> Result<(), Error> {
        self.intro_type(sym, typing.typ)?;
        if typing.rewritable {
            let rules = match typing.term {
                None => Vec::new(),
                Some((tm, _check)) => vec![Rule {
                    ctx: Vec::new(),
                    lhs: TopPattern::from(sym),
                    rhs: tm,
                }],
            };
            self.intro_rules(sym, rules)?;
        }
        Ok(())
    }
}
