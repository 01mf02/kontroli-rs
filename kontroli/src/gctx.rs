//! Maps from symbols to their associated types and rewrite rules.

use alloc::{string::String, sync::Arc, vec::Vec};
use core::hash::Hash;

/// Immutable HashMap for fast cloning of global contexts.
type FnvHashMap<K, V> = im::hashmap::HashMap<K, V, fnv::FnvBuildHasher>;

type Rule<Sym, Pat, Tm> = crate::Rule<(String, Option<Tm>), crate::App<Sym, Pat>, Tm>;

type Typing<Tm> = crate::Typing<Tm, Option<Tm>>;

#[derive(Debug)]
pub enum Error {
    Reintroduction,
    NonRewritable,
}

/// Map from symbols to their associated types and rewrite rules.
///
/// Furthermore, set whether convertibility should be checked modulo eta.
#[derive(Clone)]
pub struct GCtx<Sym, Pat, Tm> {
    types: FnvHashMap<Sym, Arc<Tm>>,
    rules: FnvHashMap<Sym, Arc<Vec<Rule<Sym, Pat, Tm>>>>,
    pub eta: bool,
}

impl<Sym, Pat, Tm> Default for GCtx<Sym, Pat, Tm> {
    fn default() -> Self {
        Self {
            types: Default::default(),
            rules: Default::default(),
            eta: false,
        }
    }
}

impl<Sym: Clone + Eq + Hash, Pat: Clone, Tm: Clone> GCtx<Sym, Pat, Tm> {
    /// Construct an empty global context without eta modularity.
    ///
    /// ~~~
    /// # use kontroli::kernel::GCtx;
    /// let gc = GCtx::new();
    /// assert!(gc.eta == false);
    /// ~~~
    pub fn new() -> Self {
        Self::default()
    }

    /// Return the type of a symbol.
    pub fn get_type(&self, sym: &Sym) -> Option<&Arc<Tm>> {
        self.types.get(sym)
    }

    /// Return the rewrite rules whose left-hand side head is the given symbol.
    pub fn get_rules(&self, sym: &Sym) -> Option<&Arc<Vec<Rule<Sym, Pat, Tm>>>> {
        self.rules.get(sym)
    }

    fn intro_type(&mut self, sym: Sym, typ: Tm) -> Result<(), Error> {
        if self.types.insert(sym, Arc::new(typ)).is_some() {
            return Err(Error::Reintroduction);
        }
        Ok(())
    }

    fn intro_rules(&mut self, sym: Sym, rules: Vec<Rule<Sym, Pat, Tm>>) -> Result<(), Error> {
        if self.rules.insert(sym, Arc::new(rules)).is_some() {
            return Err(Error::Reintroduction);
        }
        Ok(())
    }

    /// Add a rewrite rule to an existing symbol.
    pub fn add_rule(&mut self, rule: Rule<Sym, Pat, Tm>) -> Result<(), Error> {
        let rules = self
            .rules
            .get_mut(&rule.lhs.symbol)
            .ok_or(Error::NonRewritable)?;
        Arc::make_mut(rules).push(rule);
        Ok(())
    }

    /// Introduce a new symbol with given typing.
    pub fn insert(&mut self, sym: Sym, typing: Typing<Tm>, rewritable: bool) -> Result<(), Error> {
        self.intro_type(sym.clone(), typing.ty)?;
        if rewritable {
            let rules = match typing.tm {
                None => Vec::new(),
                Some(tm) => Vec::from([Rule {
                    ctx: Vec::new(),
                    lhs: sym.clone().into(),
                    rhs: tm,
                }]),
            };
            self.intro_rules(sym, rules)?;
        }
        Ok(())
    }
}
