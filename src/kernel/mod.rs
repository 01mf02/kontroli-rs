#[path = "../scope/command.rs"]
pub mod command;
mod convertible;
mod matching;
#[path = "../scope/pattern.rs"]
pub mod pattern;
mod reduce;
#[path = "../scope/rule.rs"]
pub mod rule;
mod share;
pub mod signature;
pub mod state;
mod subst;
pub mod term;
pub mod typing;

use super::Rc;

pub use command::Command;
pub use pattern::Pattern;
pub use rule::Rule;
pub use signature::Signature;
pub use term::{RTerm, Term};
pub use typing::Typing;

use crate::error::Error;
use crate::pre::{self, parse::parse};
use crate::scope::{Symbol, Symbols};

impl<'s> Command<'s> {
    /// Parse a command and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols<'s>) -> Result<Self, Error> {
        Ok(Self::from(parse::<pre::Command>(i)?.scope(&syms)?))
    }
}

impl<'s> Term<'s> {
    /// Parse a term and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols<'s>) -> Result<Self, Error> {
        Ok(Self::from(parse::<pre::Term>(i)?.scope(&syms)?))
    }
}

impl<'s> Rule<'s> {
    /// Parse a rule and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols<'s>) -> Result<Self, Error> {
        Ok(Self::from(parse::<pre::Rule>(i)?.scope(&syms)?))
    }
}
