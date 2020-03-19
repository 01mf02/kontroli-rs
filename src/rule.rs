//! Rewrite rules.

use crate::pattern::{Pattern, TopPattern};
use crate::prerule::GRule;
use crate::term::RTerm;
use std::fmt;

pub type Rule = GRule<String, TopPattern, RTerm>;

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pat = Pattern::from(self.lhs.clone());
        write!(f, "{} ⟶ {}", &pat, self.rhs)
    }
}
