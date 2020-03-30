#![no_std]

//! Type checking for the lambda-Pi calculus modulo rewriting.
//!
//! This is the library underlying the [Kontroli] proof checker.
//!
//! Users communicate with Kontroli using *commands*.
//! A command either
//! introduces of a new name (by declaration, definition, or theorem), or
//! adds a rewrite rule.
//! The state of a Kontroli typechecking session consists of
//! a [Symbols] table, keeping track of all previously introduced names, and
//! a [Signature], recording types and rewrite rules attached to symbols.
//!
//! How is a user command processed?
//! A command is parsed from a string to yield a [Precommand].
//! The scoping operation then refines the precommand to a [Command],
//! verifying whether the names referenced in the precommand
//! have been previously declared in the [Symbols] table.
//! Once we have a command, we distinguish whether it
//! introduces a name or adds a rewrite rule:
//! In case of a rewrite rule, we add the rewrite rule to the signature.
//! In case of a name introduction, we first
//! update the [Symbols] table with the newly introduced name and
//! verify that the given types and terms are valid by typing them,
//! yielding a signature [Entry].
//! Once we have an entry, we add it to the signature.
//!
//! [Kontroli]: https://github.com/01mf02/kontroli-rs
//!
//! [Symbols]: symbols/struct.Symbols.html
//! [Signature]: signature/struct.Signature.html
//! [Precommand]: precommand/enum.Precommand.html
//! [Command]: command/enum.Command.html
//! [Entry]: signature/struct.Entry.html

extern crate alloc;
extern crate lazy_st;
extern crate nom;
#[macro_use]
extern crate log;

pub mod command;
pub mod error;
pub mod parse;
pub mod pattern;
pub mod precommand;
pub mod prepattern;
pub mod prerule;
pub mod preterm;
pub mod reduce;
pub mod rule;
pub mod scope;
pub mod signature;
pub mod stack;
pub mod subst;
pub mod symbol;
pub mod symbols;
pub mod term;
pub mod typing;

pub use command::Command;
pub use error::Error;
pub use pattern::Pattern;
pub use precommand::Precommand;
pub use prepattern::Prepattern;
pub use prerule::Prerule;
pub use preterm::Preterm;
pub use rule::Rule;
pub use signature::Signature;
pub use symbol::Symbol;
pub use symbols::Symbols;
pub use term::{RTerm, Term};

impl Command {
    /// Parse a command and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols) -> Result<Self, Error> {
        Ok(parse::parse::<Precommand>(i)?.scope(&syms)?)
    }
}

impl Term {
    /// Parse a term and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols) -> Result<Self, Error> {
        Ok(parse::parse::<Preterm>(i)?.scope_closed(&syms)?)
    }
}

impl Rule {
    /// Parse a rule and scope it. Used for testing.
    pub fn parse(i: &str, syms: &Symbols) -> Result<Self, Error> {
        Ok(parse::parse::<Prerule>(i)?.scope(&syms)?)
    }
}
