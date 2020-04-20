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
pub struct Signature {
    pub types: FnvHashMap<Symbol, RTerm>,
    pub rules: FnvHashMap<Symbol, Vec<Rule>>,
    pub eta: bool,
}

impl Signature {
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

    fn intro_type(&mut self, sym: Symbol, typ: RTerm) -> Result<(), Error> {
        if self.types.insert(sym, typ).is_some() {
            return Err(Error::Reintroduction);
        }
        Ok(())
    }

    fn intro_rules(&mut self, sym: Symbol, rules: Vec<Rule>) -> Result<(), Error> {
        if self.rules.insert(sym, rules).is_some() {
            return Err(Error::Reintroduction);
        }
        Ok(())
    }

    /// Add a rewrite rule to an existing symbol.
    pub fn add_rule(&mut self, rule: Rule) -> Result<(), Error> {
        self.rules
            .get_mut(&rule.lhs.symbol)
            .ok_or(Error::NonRewritable)?
            .push(rule);
        Ok(())
    }

    /// Introduce a new symbol with given typing.
    pub fn insert(&mut self, sym: &Symbol, typing: Typing) -> Result<(), Error> {
        self.intro_type(sym.clone(), typing.typ)?;
        if typing.rewritable {
            let rules = match typing.term {
                None => Vec::new(),
                Some((tm, _check)) => vec![Rule {
                    ctx: Vec::new(),
                    lhs: TopPattern::from(Symbol::clone(sym)),
                    rhs: tm,
                }],
            };
            self.intro_rules(sym.clone(), rules)?;
        }
        Ok(())
    }
}
