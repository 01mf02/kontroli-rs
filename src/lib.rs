//! Type checking for the lambda-Pi calculus modulo rewriting.

extern crate circular;
extern crate lazy_st;
extern crate nom;
#[macro_use]
extern crate log;

pub mod command;
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
pub mod term;
pub mod typing;
