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
pub mod constant;
pub mod lex;
mod period;
pub mod term;

pub use cmd::{Command, Intro, Rule};
pub use cmditer::{CmdIter, Error};
pub use constant::Constant;
pub use lex::Token;
pub use period::Period;
pub use term::Term;

use alloc::vec::Vec;
use core::borrow::Borrow;
use logos::Logos;

pub trait Consts<C> {
    fn get<S: Borrow<str>>(&self, path: &[S], name: &S) -> Option<C>;
}

pub fn lex(s: &str) -> impl Iterator<Item = Token<&str>> {
    Token::lexer(s).filter(|token| *token != Token::Space)
}

pub fn period<S>(iter: &mut impl Iterator<Item = Token<S>>, tokens: &mut Vec<Token<S>>) {
    while let Some(token) = iter.next() {
        match token {
            Token::Space | Token::Comment(0) => (),
            Token::Dot => match iter.next() {
                Some(Token::Space) | Some(Token::Comment(0)) | None => {
                    tokens.push(Token::Period);
                    return;
                }
                Some(other) => tokens.extend([Token::Dot, other]),
            },
            _ => tokens.push(token),
        }
    }
}
