#![no_std]

extern crate alloc;

pub mod lex;
pub mod parse;

pub use lex::Token;
pub use parse::{Command, Error, Intro, Parse, Rule, Term};

use logos::Logos;

pub fn lex<'s>(s: &'s str) -> impl Iterator<Item = Token<'s>> {
    Token::lexer(s).filter(|token| *token != Token::Space)
}

#[cfg(feature = "itertools")]
use alloc::vec::Vec;

#[cfg(feature = "itertools")]
pub fn lexes<'s>(s: &'s str) -> impl Iterator<Item = Result<Vec<Token<'s>>, Error>> {
    use itertools::Itertools;
    Token::lexer(s).batching(parse::until_period)
}
