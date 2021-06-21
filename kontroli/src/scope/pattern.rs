//! Rewrite patterns.

use super::Symbol;
pub use crate::pattern::Miller;

/// Rewrite pattern.
pub type Pattern<'s> = crate::Pattern<Symbol<'s>>;

/// Pattern at the left-hand side of a rewrite rule.
pub type TopPattern<'s> = crate::pattern::TopPattern<Symbol<'s>>;
