#![no_std]

extern crate alloc;

pub mod lex;
pub mod parse;

pub use lex::Token;
pub use parse::{Command, Intro, Rule, Term};
