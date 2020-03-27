#![no_std]

//! Type checking for the lambda-Pi calculus modulo rewriting.
//!
//! Kontroli (Esperanto for *verify*) is
//! an alternative implementation of the logical framework [Dedukti],
//! concentrating on the verification of proofs.
//! Kontroli's use case is to provide a "second opinion" to Dedukti and
//! to make it easier for users to understand and learn from the implementation.
//! It is also a testbed for parallelising type checking.
//!
//! # Goals
//!
//! Kontroli tries to be the following:
//!
//! * Small: write as little code as possible ...
//! * Correct: ... because the code you do not write, contains no bugs
//! * Efficient: be at least as fast as Dedukti in main use case
//! * Compatible: stick to Dedukti's syntax/semantics as much as possible
//! * Conservative: use established and well-tested techniques
//!
//! # Differences
//!
//! There are a few differences with respect to Dedukti:
//!
//! * Kontroli's syntax is not left-recursive, in order to simplify parsing.
//!   As a result, it cannot directly read most of today's Dedukti files, but
//!   converting Kontroli to Dedukti files is only a matter of a `sed` one-liner.
//!   (Dedukti could be also easily extended to read/write Kontroli syntax.)
//! * Kontroli does not support higher-order rewrite rules,
//!   as they would make the whole program considerably more complex,
//!   thus contradicting the idea of a small type checker.
//! * Kontroli does not try to assure the type-safety of rewrite rules.
//!   However, using Kontroli's API, it should be relatively straightforward
//!   to implement a type checker that assures type-safety.
//! * Kontroli does not use decision trees for rewriting.
//! * Kontroli does not have any commands like `#EVAL` or `#ASSERT`,
//!   which are particularly used in Dedukti tests.
//!   Instead, tests in the code base are preferred.
//!
//! # Usage
//!
//! Kontroli provides a command-line program and a library.
//! The latter means that you can use Kontroli as part of your own applications.
//! Given that the Kontroli library does not rely on Rust's standard library,
//! you could use it also in environments such as web pages.
//!
//! [Dedukti]: https://deducteam.github.io/

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

pub use error::Error;
pub use pattern::Pattern;
pub use prepattern::Prepattern;
pub use prerule::Prerule;
pub use preterm::Preterm;
pub use rule::Rule;
pub use signature::Signature;
pub use symbol::Symbol;
pub use symbols::Symbols;
pub use term::{RTerm, Term};

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
