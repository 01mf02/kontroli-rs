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
//! a global context [`GCtx`], recording types and rewrite rules attached to symbols.
//!
//! How is a user command processed?
//! A command is parsed from a string to yield a [`parse::Command`].
//! The scoping operation then refines the parse command to a [`scope::Command`],
//! verifying whether the names referenced in the parse command
//! have been previously declared in the [`Symbols`] table.
//! Once we have a scope command, we distinguish whether it
//! introduces a name or adds a rewrite rule:
//! In case of a rewrite rule, we add the rewrite rule to the global context.
//! In case of a name introduction, we first
//! update the [`Symbols`] table with the newly introduced name and
//! verify that the given types and terms are valid, yielding a [`Typing`].
//! Once we have a typing, we add it to the global context.
//!
//! The following example parses a few commands and executes them on a global context.
//! (By the way, this example, just as all other code examples in this library,
//! can be executed by running `cargo test`.)
//!
//! ~~~
//! # use kontroli::{Command, Error, Share, Symbols};
//! # use kontroli::rc::{GCtx, Intro, Rule, Typing};
//! # use colosseum::unsync::Arena;
//! let cmds = [
//!     // declarations
//!     "prop : Type.",
//!     "imp : prop -> prop -> prop.",
//!
//!     // definition with a rewrite rule
//!     "def proof : prop -> Type.",
//!     "[x: prop, y: prop] proof (imp x y) --> proof x -> proof y.",
//!
//!     // theorem
//!     r"thm imp_refl (x : prop) : proof (imp x x) := p : proof x => p.",
//! ];
//!
//! let arena = Arena::new();
//! let mut syms = Symbols::new();
//! let mut gc = GCtx::new();
//!
//! for c in cmds.iter() {
//!     // parse and scope command in one go
//!     let cmd = Command::parse(c);
//!     match cmd {
//!         // introduction of a new name
//!         Command::Intro(id, it) => {
//!             let it: Intro = it.share(&syms)?;
//!
//!             let id: &str = arena.alloc(id);
//!             // add symbol to symbol table and fail if it is not new
//!             let sym = syms.insert(id)?;
//!
//!             // typecheck and insert into global context
//!             let rewritable = it.rewritable();
//!             let typing: Typing = Typing::intro(it, &gc)?;
//!             typing.check(&gc)?;
//!             gc.insert(sym, typing, rewritable)?
//!         }
//!         // addition of rewrite rules
//!         Command::Rules(rules) => {
//!             for rule in rules {
//!                 gc.add_rule(rule.share(&syms)?)?
//!             }
//!         }
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
#[macro_use]
extern crate log;

pub use dedukti_parse as parse;

mod app;
mod command;
pub mod error;
mod gctx;
mod intro;
pub mod kernel;
pub mod lterm;
mod pattern;
mod rule;
mod share;
pub mod symbol;
mod symbols;
mod typing;

pub use app::App;
pub use command::Command;
pub use error::Error;
pub use gctx::GCtx;
pub use intro::Intro;
pub use pattern::Pattern;
pub use rule::Rule;
pub use share::Share;
pub use symbol::Symbol;
pub use symbols::Symbols;
pub use typing::Typing;
