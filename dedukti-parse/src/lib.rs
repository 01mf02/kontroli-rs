//! Parser for the Dedukti file format
//!
//! Example usage:
//!
//! ~~~
//! use dedukti_parse::{CmdIter, Command, Error, Symb};
//!
//! let cmds = "prop: Type. def proof : prop -> Type.";
//! let cmds = CmdIter::<_, Symb<&str>, &str>::new(&cmds);
//! let cmds: Result<Vec<_>, _> = cmds.collect();
//! assert_eq!(cmds?.len(), 2);
//! # Ok::<_, Error>(())
//! ~~~
#![no_std]

extern crate alloc;

pub mod cmd;
mod cmditer;
pub mod lex;
pub mod symb;
pub mod term;

pub use cmd::{Command, Intro, Rule};
pub use cmditer::{CmdIter, Error, Lazy};
pub use lex::Token;
pub use symb::Symb;
pub use term::{Atom, Scope, Term};

pub use logos::Logos as Lex;
