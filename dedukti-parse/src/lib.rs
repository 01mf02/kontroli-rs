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
pub mod symb;
pub mod lex;
pub mod term;

pub use cmd::{Command, Intro, Rule};
pub use cmditer::{CmdIter, Error};
pub use symb::Symb;
pub use lex::Token;
pub use term::Term;

pub use logos::Logos as Lex;
