//! Unscoped rewrite rules.

use super::Preterm;
use alloc::{string::String, vec::Vec};

/// Generalised rewrite rules.
///
/// They consist of:
///
/// * a context (bound variables),
/// * a left-hand side (pattern to match with), and
/// * a right-hand side (term to replace with).
#[derive(Clone, Debug)]
pub struct GRule<V, L, R> {
    pub ctx: Vec<V>,
    pub lhs: L,
    pub rhs: R,
}

/// Rewrite rules with strings as bound variable identifiers,
/// and preterms as left- and right-hand sides.
///
/// This is a vast overapproximation of rules, because
/// not every preterm is a valid rule left-hand side.
/// Scoping takes care to separate the wheat from the chaff.
pub type Prerule = GRule<String, Preterm, Preterm>;
