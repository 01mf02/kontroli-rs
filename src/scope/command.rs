//! Signature-changing commands.

use super::{Intro, Rule};

/// Signature-changing command.
pub enum Command<'s, Id> {
    /// Introduce a new name
    Intro(Id, Intro<'s>),
    /// Add a rewrite rule
    Rule(Rule<'s>),
}
