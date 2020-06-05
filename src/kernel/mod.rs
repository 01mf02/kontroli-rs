pub mod command;
mod convertible;
mod matching;
pub mod pattern;
mod reduce;
pub mod rule;
pub mod scope;
pub mod signature;
pub mod state;
mod subst;
pub mod symbol;
pub mod symbols;
pub mod term;
pub mod typing;

use super::Rc;

pub use command::Command;
pub use pattern::Pattern;
pub use rule::Rule;
pub use signature::Signature;
pub use symbol::Symbol;
pub use symbols::Symbols;
pub use term::{RTerm, Term};
pub use typing::Typing;

use crate::error::Error;
use crate::pre;
use crate::pre::parse::parse;

impl<'s> Command<'s> {
    /// Parse a command and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols<'s>) -> Result<Self, Error> {
        Ok(Self::scope(parse::<pre::Command>(i)?, &syms)?)
    }
}

impl<'s> Term<'s> {
    /// Parse a term and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols<'s>) -> Result<Self, Error> {
        Ok(Self::scope(parse::<pre::Term>(i)?, &syms)?)
    }
}

impl<'s> Rule<'s> {
    /// Parse a rule and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols<'s>) -> Result<Self, Error> {
        Ok(Self::scope(parse::<pre::Rule>(i)?, &syms)?)
    }
}
