use alloc::vec::Vec;
use core::fmt::{self, Display};

/// Signature-changing command.
#[derive(Clone)]
pub enum Command<Id, Intro, Rule> {
    /// Introduce a new name
    Intro(Id, Intro),
    /// Add rewrite rules
    Rules(Vec<Rule>),
}

impl<Id: Display, Intro: Display, Rule: Display> Display for Command<Id, Intro, Rule> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Intro(id, it) => write!(f, "{} {}", id, it),
            Self::Rules(rules) => rules.iter().try_for_each(|r| r.fmt(f)),
        }
    }
}
