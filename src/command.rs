use core::fmt::{self, Display};

/// Signature-changing command.
#[derive(Clone)]
pub enum Command<Id, Intro, Rule> {
    /// Introduce a new name
    Intro(Id, Intro),
    /// Add a rewrite rule
    Rule(Rule),
}

impl<Id: Display, Intro: Display, Rule: Display> Display for Command<Id, Intro, Rule> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Intro(id, it) => write!(f, "{} {}", id, it),
            Self::Rule(rule) => rule.fmt(f),
        }
    }
}
