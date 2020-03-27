//! Type checking for the lambda-Pi calculus modulo rewriting.
//!
//! Kontroli (Esperanto for *verify*) is
//! an alternative implementation of the logical framework [Dedukti],
//! concentrating on the verification of proofs.
//! Kontroli's use case is to provide a "second opinion" to Dedukti and
//! to make it easier for users to understand and learn from the implementation.
//! It is also a testbed for parallelising type checking.
//!
//! It tries to be the following:
//!
//! * Small: write as little code as possible ...
//! * Correct: ... because the code you do not write, contains no bugs
//! * Efficient: be at least as fast as Dedukti in main use case
//! * Compatible: stick to Dedukti's syntax/semantics as much as possible
//! * Conservative: use established and well-tested techniques
//!
//! There are a few differences with respect to Dedukti:
//!
//! * Kontroli's syntax is not left-recursive to simplify the parser.
//!   As a result, most of today's Dedukti files cannot be directly read,
//!   but it is relatively simple to convert Kontroli to Dedukti files.
//!   (Dedukti could be also easily extended to read/write Kontroli syntax.)
//! * Kontroli does not support higher-order rewrite rules,
//!   as they make the whole program considerably more complex,
//!   thus contradicting the idea of a small type checker.
//! * Kontroli does not try to assure the type-safety of rewrite rules.
//!   However, using Kontroli's API, it should be relatively straightforward
//!   to implement a type checker that assures type-safety.
//! * Kontroli does not use decision trees for rewriting.
//! * Kontroli does not have any commands like `#EVAL` or `#ASSERT`,
//!   which are particularly used in Dedukti tests.
//!   Instead, tests in the code base are preferred.
//!
//! [Dedukti]: https://deducteam.github.io/

extern crate circular;
extern crate lazy_st;
extern crate nom;
#[macro_use]
extern crate log;

pub mod command;
pub mod error;
pub mod parse;
pub mod parsebuffer;
pub mod parseerror;
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
