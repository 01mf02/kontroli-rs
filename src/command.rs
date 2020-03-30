//! Signature-changing commands.

use crate::precommand::GIntroType;
use crate::rule::Rule;
use crate::term::RTerm;
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
