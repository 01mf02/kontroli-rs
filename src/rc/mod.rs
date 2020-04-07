pub mod command;
mod convertible;
pub mod error;
mod matching;
pub mod pattern;
mod reduce;
pub mod rule;
pub mod scope;
mod sharing;
pub mod signature;
pub mod state;
mod subst;
pub mod symbol;
pub mod symbols;
pub mod term;
pub mod typing;

pub use command::Command;
pub use error::Error;
pub use pattern::Pattern;
pub use rule::Rule;
pub use signature::Signature;
pub use symbol::Symbol;
pub use symbols::Symbols;
pub use term::{RTerm, Term};

use crate::pre::parse::parse;
use crate::pre::{Precommand, Prerule, Preterm};

impl Command {
    /// Parse a command and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols) -> Result<Self, Error> {
        Ok(Self::scope(parse::<Precommand>(i)?, &syms)?)
    }
}

impl Term {
    /// Parse a term and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols) -> Result<Self, Error> {
        Ok(Self::scope(parse::<Preterm>(i)?, &syms)?)
    }
}

impl Rule {
    /// Parse a rule and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols) -> Result<Self, Error> {
        Ok(Self::scope(parse::<Prerule>(i)?, &syms)?)
    }
}
