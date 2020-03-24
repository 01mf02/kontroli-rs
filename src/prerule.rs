//! Unscoped rewrite rules.

use crate::preterm::Preterm;

#[derive(Clone)]
pub struct GRule<V, L, R> {
    pub ctx: Vec<V>,
    pub lhs: L,
    pub rhs: R,
}

pub type Prerule = GRule<String, Preterm, Preterm>;
