//! Parser for the Dedukti file format
//!
//! Example usage:
//!
//! ~~~
//! use dedukti_parse::{CmdIter, Command, Error};
//!
//! let cmds = "prop: Type. def proof : prop -> Type.";
//! let cmds = CmdIter::new(&cmds);
//! let cmds: Result<Vec<_>, _> = cmds.collect();
//! assert_eq!(cmds?.len(), 2);
//! # Ok::<_, Error>(())
//! ~~~
#![no_std]

extern crate alloc;

pub mod cmd;
mod cmditer;
pub mod lex;
pub mod term;

pub use cmd::{Command, Intro, Rule};
pub use cmditer::{CmdIter, Error};
pub use lex::Token;
pub use term::{Term, TermB};

use alloc::vec::Vec;
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
