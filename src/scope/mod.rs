pub mod command;
pub mod intro;
pub mod pattern;
pub mod rterm;
mod rule;
mod scope;
mod symbol;
mod symbols;
pub mod term;

pub use command::Command;
pub use intro::Intro;
pub use pattern::Pattern;
pub use rterm::RTerm;
pub use rule::Rule;
pub use symbol::Symbol;
pub use symbols::Symbols;
pub use term::Term;

use crate::error::Error;
use crate::pre::{self, parse::parse};

impl<'s> Command<'s, alloc::string::String> {
    /// Parse a command and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols<'s>) -> Result<Self, Error> {
        Ok(parse::<pre::Command>(i)?.scope(&syms)?)
    }
}

impl<'s> Term<'s> {
    /// Parse a term and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols<'s>) -> Result<Self, Error> {
        Ok(parse::<pre::Term>(i)?.scope(&syms)?)
    }
}

impl<'s> Rule<'s> {
    /// Parse a rule and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols<'s>) -> Result<Self, Error> {
        Ok(parse::<pre::Rule>(i)?.scope(&syms)?)
    }
}
