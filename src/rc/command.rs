//! Signature-changing commands.

use super::{RTerm, Rule};
use crate::pre::precommand::GIntroType;
use alloc::string::String;

/// Signature-changing command.
pub enum Command {
    /// Introduce a new name
    Intro(String, IntroType),
    /// Add a rewrite rule
    Rule(Rule),
}

/// The way we introduce a new name.
pub type IntroType = GIntroType<RTerm, RTerm>;
