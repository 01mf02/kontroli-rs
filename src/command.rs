//! Commands to change the signature.

use crate::precommand::GIntroType;
use crate::rule::Rule;
use crate::symbol::Symbol;
use crate::term::RTerm;

pub enum Command {
    Intro(Symbol, IntroType),
    Rule(Rule),
}

pub type IntroType = GIntroType<RTerm, RTerm>;
