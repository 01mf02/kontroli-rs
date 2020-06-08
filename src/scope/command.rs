//! Signature-changing commands.

use super::{RTerm, Rule};
use crate::pre::command::GIntroType;
use alloc::string::String;

/// Signature-changing command.
pub enum Command<'s> {
    /// Introduce a new name
    Intro(String, IntroType<'s>),
    /// Add a rewrite rule
    Rule(Rule<'s>),
}

/// The way we introduce a new name.
pub type IntroType<'s> = GIntroType<RTerm<'s>, RTerm<'s>>;
