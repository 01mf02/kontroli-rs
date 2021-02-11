#![no_std]
#![forbid(unsafe_code)]

//! Type checking for the lambda-Pi calculus modulo rewriting.
//!
//! This is the library underlying the [Kontroli] proof checker.
//!
//! # Usage
//!
//! Users communicate with Kontroli using *commands*.
//! A command either
//! introduces of a new name (by declaration, definition, or theorem), or
//! adds a rewrite rule.
//! The state of a Kontroli typechecking session consists of
//! a [`Symbols`] table, keeping track of all previously introduced names, and
//! a [`Signature`], recording types and rewrite rules attached to symbols.
//!
//! How is a user command processed?
//! A command is parsed from a string to yield a [`parse::Command`].
//! The scoping operation then refines the parse command to a [`scope::Command`],
//! verifying whether the names referenced in the parse command
//! have been previously declared in the [`Symbols`] table.
//! Once we have a scope command, we distinguish whether it
//! introduces a name or adds a rewrite rule:
//! In case of a rewrite rule, we add the rewrite rule to the signature.
//! In case of a name introduction, we first
//! update the [`Symbols`] table with the newly introduced name and
//! verify that the given types and terms are valid, yielding a [`Typing`].
//! Once we have a typing, we add it to the signature.
//!
//! The following example parses a few commands and executes them on a signature.
//! (By the way, this example, just as all other code examples in this library,
//! can be executed by running `cargo test`.)
//!
//! ~~~
//! # use kontroli::Error;
//! # use kontroli::scope::{Command, Symbols};
//! # use kontroli::rc::{Intro, Rule, Signature, Typing};
//! # use colosseum::unsync::Arena;
//! let cmds = [
//!     // declarations
//!     "prop : Type.\n",
//!     "imp : prop -> prop -> prop.\n",
//!
//!     // definition with a rewrite rule
//!     "def proof : prop -> Type.\n",
//!     "[x, y] proof (imp x y) --> proof x -> proof y.\n",
//!
//!     // theorem
//!     r"thm imp_refl (x : prop) : proof (imp x x) := p : proof x => p.\n",
//! ];
//!
//! let arena = Arena::new();
//! let mut syms = Symbols::new();
//! let mut sig = Signature::new();
//!
//! for c in cmds.iter() {
//!     // parse and scope command in one go
//!     let cmd: Command<String> = Command::parse(c, &syms)?;
//!     match cmd {
//!         // introduction of a new name
//!         Command::Intro(id, it) => {
//!             let id: &str = arena.alloc(id);
//!             // add symbol to symbol table and fail if it is not new
//!             let sym = syms.insert(id)?;
//!
//!             // typecheck and insert into signature
//!             let typing: Typing = Typing::new(Intro::from(it), &sig)?.check(&sig)?;
//!             sig.insert(sym, typing)?
//!         }
//!         // addition of rewrite rules
//!         Command::Rules(rules) => sig.add_rules(rules.into_iter().map(Rule::from))?
//!     }
//! }
//! # Ok::<_, Error>(())
//! ~~~
//!
//! # Organisation
//!
//! This library is divided into several modules:
//! * The [`parse`] module contains unshared, reference-free data structures,
//! * the [`scope`] module contains data structures with references, and
//! * the [`rc`] and [`arc`] modules contain data structures with references and shared pointers.
//!
//! The [`rc`] and [`arc`] modules expose completely the same API,
//! the difference being that the structures in [`rc`]
//! cannot be used in multi-threaded scenarios.
//! Due to the performance overhead incurred by the data structures in [`arc`],
//! it is advisable to use these only in multi-threaded scenarios,
//! and to prefer [`rc`] whenever possible.
//!
//! For many data structures, we have counterparts in
//! the [`parse`], [`scope`], and [`rc`]/[`arc`] modules.
//! We call types from the [`parse`] and [`scope`] modules
//! "parse structures" and "scope structures", respectively.
//! For example, we distinguish parse terms, scope terms, and terms
//! (the latter being defined in the [`rc`]/[`arc`] modules).
//! Parse structures are constructed by the parser and
//! refined into their corresponding scope structures by the scoper.
//! Parse and scope structures also implement the `Send` and `Sync` traits,
//! meaning that they can be transferred and shared between threads.
//! This allows parsing and checking to be performed in parallel.
//!
//! [Kontroli]: https://github.com/01mf02/kontroli-rs
//!
//! [`Symbols`]: scope::Symbols

extern crate alloc;
extern crate lazy_st;
extern crate nom;
#[macro_use]
extern crate log;

pub mod parse;
pub mod scope;

/// Multi-threading kernel.
#[cfg(not(doctest))]
pub mod arc {
    use alloc::sync::Arc as Rc;
    #[path = "../kernel/mod.rs"]
    mod kernel;
    pub use kernel::*;
}
/// Single-threading kernel.
pub mod rc {
    use alloc::rc::Rc;
    #[path = "../kernel/mod.rs"]
    mod kernel;
    pub use kernel::*;
}

mod application;
mod arg;
mod command;
pub mod error;
mod intro;
mod pattern;
mod rule;
mod signature;
mod stack;
mod term;
mod typing;

pub use application::Application;
pub use arg::Arg;
pub use command::Command;
pub use error::Error;
pub use intro::Intro;
pub use pattern::Pattern;
pub use rule::Rule;
pub use signature::Signature;
pub use stack::Stack;
pub use term::Term;
pub use typing::Typing;
