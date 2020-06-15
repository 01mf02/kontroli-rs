//! Signature-changing commands.

use super::{Intro, Rule};

/// Signature-changing command.
pub enum Command<'s, Id> {
    /// Introduce a new name
    Intro(Id, Intro<'s>),
    /// Add a rewrite rule
    Rule(Rule<'s>),
}

impl<'s, Id> Command<'s, Id> {
    pub fn map_id_err<F, Id2, E>(self, f: F) -> Result<Command<'s, Id2>, E>
    where
        F: FnOnce(Id) -> Result<Id2, E>,
    {
        match self {
            Self::Intro(id, it) => Ok(Command::Intro(f(id)?, it)),
            Self::Rule(rule) => Ok(Command::Rule(rule)),
        }
    }
}
