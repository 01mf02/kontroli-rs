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
//! Once we have a command, we distinguish whether it
//! introduces a name or adds a rewrite rule:
//! In case of a rewrite rule, we add the rewrite rule to the global context.
//! In case of a name introduction, we first
//! update the [`Symbols`] table with the newly introduced name and
//! verify that the given types and terms are valid, yielding a [`Typing`].
//! In both cases, [`share`](share::Share::share) refines terms in a command to shared terms,
//! verifying whether the names referenced in the terms,
//! have been previously declared in the [`Symbols`] table.
//! Once we have a typing, we add it to the global context.
//!
//! The following example parses a few commands and executes them on a global context.
//! (By the way, this example, just as all other code examples in this library,
//! can be executed by running `cargo test`.)
//!
//! ~~~
//! # use kontroli::{Command, Error, Share, Symbols};
//! # use kontroli::kernel::{self, GCtx};
//! # use colosseum::unsync::Arena;
//! let cmds = r#"
//!     (; declarations ;)
//!     prop : Type.
//!     imp : prop -> prop -> prop.
//!
//!     (; definition with a rewrite rule ;)
//!     def proof : prop -> Type.
//!     [x: prop, y: prop] proof (imp x y) --> proof x -> proof y.
//!
//!     (; theorem ;)
//!     thm imp_refl (x : prop) : proof (imp x x) := p : proof x => p.
//! "#;
//!
//! let arena = Arena::new();
//! let mut syms = Symbols::new();
//! let mut gc = GCtx::new();
//!
//! for cmd in kontroli::parse::Strict::new(cmds) {
//!     // match constants in the command to previously introduced constants
//!     let cmd = cmd.unwrap().share(&syms)?;
//!     match cmd {
//!         // introduction of a new name
//!         Command::Intro(id, it) => {
//!             let owned = kontroli::symbol::Owned::new(id.clone());
//!             // add symbol to symbol table and fail if it is not new
//!             let sym = syms.insert(id, arena.alloc(owned))?;
//!
//!             // typecheck and insert into global context
//!             let rewritable = it.rewritable();
//!             let (typing, check) = kernel::intro(it, &gc)?;
//!             if let Some(check) = check {
//!                 check.check(&gc)?
//!             }
//!             gc.insert(sym, typing, rewritable)?
//!         }
//!         // addition of rewrite rules (without typechecking them!)
//!         Command::Rules(rules) => rules.into_iter().try_for_each(|r| gc.add_rule(r))?,
//!     }
//! }
//! # Ok::<_, Error>(())
//! ~~~
//!
//! # Organisation
//!
//! This library is divided into several modules:
//! * The [`parse`] module contains unshared, reference-free data structures,
//! * the [`share`] module contains data structures with references, and
//! * the [`kernel`] module contains data structures with references and shared pointers.
//!
//! For many data structures, we have counterparts in
//! the [`parse`], [`share`], and [`kernel`] modules.
//! We call types from the [`parse`] and [`share`] modules
//! "parsed structures" and "shared structures", respectively.
//! For example, we distinguish parsed terms, shared terms, and (kernel) terms
//! (the latter being defined in the [`kernel`] module).
//! Parsed structures are constructed by the parser and
//! refined into their corresponding shared structures by the sharer.
//! Parsed and shared structures also implement the `Send` and `Sync` traits,
//! meaning that they can be transferred and shared between threads.
//! This allows parsing and checking to be performed in parallel.
//!
//! [Kontroli]: https://github.com/01mf02/kontroli-rs

extern crate alloc;
extern crate lazy_st;
#[macro_use]
extern crate log;

pub use dedukti_parse as parse;

mod app;
mod comb;
mod command;
pub mod error;
mod gctx;
pub mod kernel;
pub mod lterm;
mod pattern;
mod rule;
pub mod share;
pub mod symbol;
mod symbols;
mod typing;

pub use app::App;
pub use comb::Comb;
pub use command::Command;
pub use error::Error;
pub use gctx::GCtx;
pub use lterm::LTerm;
pub use parse::Intro;
pub use pattern::Pattern;
pub use rule::Rule;
pub use share::Share;
pub use symbol::Symbol;
pub use symbols::Symbols;
pub use typing::Typing;
