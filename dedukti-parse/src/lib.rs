//! Parser for the Dedukti file format
//!
//! Example usage:
//!
//! ~~~
//! use dedukti_parse::{lexes, Command, Error, Parse};
//!
//! let cmds = "prop: Type. def proof : prop -> Type.";
//! let cmds = lexes(&cmds);
//! let cmds = cmds.map(|cmd| Ok(Command::parse_vec(cmd?)));
//! let cmds: Result<Vec<_>, _> = cmds.collect();
//! assert_eq!(cmds?.len(), 2);
//! # Ok::<_, Error>(())
//! ~~~
#![no_std]

extern crate alloc;

pub mod cmd;
pub mod lex;
pub mod parse;
pub mod term;

pub use lex::Token;
pub use parse::{Command, Error, Intro, Parse, Rule, Term, TermB};

use logos::Logos;

pub fn lex(s: &str) -> impl Iterator<Item = Token<&str>> {
    Token::lexer(s).filter(|token| *token != Token::Space)
}

pub fn period<S>(iter: &mut impl Iterator<Item = Token<S>>, tokens: &mut Vec<Token<S>>) {
    while let Some(token) = iter.next() {
        match token {
            Token::Space => (),
            Token::Dot => match iter.next() {
                Some(Token::Space) | None => {
                    tokens.push(Token::Period);
                    return;
                }
                Some(other) => tokens.extend([Token::Dot, other]),
            },
            _ => tokens.push(token),
        }
    }
}

#[cfg(feature = "itertools")]
use alloc::vec::Vec;

#[cfg(feature = "itertools")]
pub fn lexes(s: &str) -> impl Iterator<Item = Result<Vec<Token<&str>>, Error>> {
    use itertools::Itertools;
    Token::lexer(s).batching(parse::until_period)
}
