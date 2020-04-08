//! Rewrite rules.

use super::pattern::{Pattern, TopPattern};
use super::RTerm;
use crate::pre::prerule::GRule;
use alloc::string::String;
use core::fmt;

/// Rewrite rules with strings as bound variable identifiers,
/// top pattern (symbol application) as left-hand side, and
/// a shared term as right-hand side.
pub type Rule = GRule<String, TopPattern, RTerm>;

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pat = Pattern::from(self.lhs.clone());
        write!(f, "{} ⟶ {}", &pat, self.rhs)
    }
}
