//! Signature-changing commands.

use crate::precommand::GIntroType;
use crate::rule::Rule;
use crate::symbol::Symbol;
use crate::term::RTerm;

/// Signature-changing command.
pub enum Command {
    /// Introduce a new name
    Intro(Symbol, IntroType),
    /// Add a rewrite rule
    Rule(Rule),
}

/// The way we introduce a new name.
pub type IntroType = GIntroType<RTerm, RTerm>;
