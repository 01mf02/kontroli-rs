#![no_std]

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
//! a [Symbols] table, keeping track of all previously introduced names, and
//! a [Signature], recording types and rewrite rules attached to symbols.
//!
//! How is a user command processed?
//! A command is parsed from a string to yield a [precommand].
//! The scoping operation then refines the precommand to a [Command],
//! verifying whether the names referenced in the precommand
//! have been previously declared in the [Symbols] table.
//! Once we have a command, we distinguish whether it
//! introduces a name or adds a rewrite rule:
//! In case of a rewrite rule, we add the rewrite rule to the signature.
//! In case of a name introduction, we first
//! update the [Symbols] table with the newly introduced name and
//! verify that the given types and terms are valid, yielding a [Typing].
//! Once we have an typing, we add it to the signature.
//! This whole process is illustrated in the following image.
//! The [Symbols] and [Signature] are boxed to indicate that
//! they persist throughout the checking.
//!
//! ~~~
//! # std::fs::create_dir_all("target/doc/kontroli")?;
//! # std::fs::copy("structure.svg", "target/doc/kontroli/structure.svg")?;
//! # Ok::<_, std::io::Error>(())
//! ~~~
//! ![Command processing.](structure.svg)
//!
//! The following example parses a few commands and executes them on a signature.
//! (By the way, this example, just as all other code examples in this library,
//! can be executed by running `cargo test`.)
//!
//! ~~~
//! # use kontroli::Error;
//! # use kontroli::scope::{Command, Symbols};
//! # use kontroli::rc::{IntroType, Rule, Signature, Typing};
//! # use colosseum::unsync::Arena;
//! let cmds = [
//!     // declarations
//!     "prop : Type.",
//!     "imp : prop -> prop -> prop.",
//!
//!     // definition with a rewrite rule
//!     "def proof : prop -> Type.",
//!     "[x, y] proof (imp x y) --> proof x -> proof y.",
//!
//!     // theorem
//!     r"thm imp_refl (x : prop) : proof (imp x x) := p : proof x => p.",
//! ];
//!
//! let arena = Arena::new();
//! let mut syms = Symbols::new();
//! let mut sig = Signature::new();
//!
//! for c in cmds.iter() {
//!     // parse to Precommand and scope to Command in one go
//!     let cmd: Command<String> = Command::parse(c, &syms)?;
//!     match cmd {
//!         // introduction of a new name
//!         Command::Intro(id, it) => {
//!             let id: &str = arena.alloc(id);
//!             // add symbol to symbol table and fail if it is not new
//!             let sym = syms.insert(id)?;
//!
//!             // typecheck and insert into signature
//!             let typing: Typing = Typing::new(IntroType::from(it), &sig)?.check(&sig)?;
//!             sig.insert(&sym, typing)?
//!         }
//!         // addition of a rewrite rule
//!         Command::Rule(rule) => sig.add_rule(Rule::from(rule))?,
//!     }
//! }
//! # Ok::<_, Error>(())
//! ~~~
//!
//! # Organisation
//!
//! This library is divided into several modules:
//! The [pre] module contains unshared data structures, whereas
//! the [rc] and [arc] modules contain shared data structures.
//! The [rc] and [arc] modules expose completely the same API,
//! the difference being that the structures in [rc]
//! cannot be used in multi-threaded scenarios.
//! Due to the performance overhead incurred by the data structures in [arc],
//! it is advisable to use these only in multi-threaded scenarios,
//! and to prefer [rc] whenever possible.
//!
//! For many data structures, we have unshared and shared counterparts.
//! We prefix the unshared counterparts with "pre-" and call it them prestructures.
//! For example, we refer to unshared and shared commands as
//! "precommands" and "commands", respectively.
//! The same for preterms and terms, prerules and rules, and so on.
//! Prestructures are constructed by the parser and
//! refined into their corresponding shared structures by `scope` functions.
//! Prestructures also implement the `Send` and `Sync` traits, meaning that
//! they can be transferred and shared between threads.
//! This allows parsing and checking to be performed in parallel.
//!
//! This library is organised around *data structures* and *functions*.
//! Some modules, such as [pattern], [term], and [state]
//! revolve around data structures of the same name and
//! define only basic functions proper to them.
//! Other modules, such as [parse] and [scope]
//! define functions that are common to multiple data structures.
//!
//! [Kontroli]: https://github.com/01mf02/kontroli-rs
//!
//! [precommand]: pre/command/enum.Command.html
//!
//! [Symbols]:   rc/symbols/struct.Symbols.html
//! [Signature]: rc/signature/struct.Signature.html
//! [Command]:   rc/command/enum.Command.html
//! [Typing]:    rc/typing/struct.Typing.html
//!
//! [pattern]: rc/pattern/index.html
//! [term]:    rc/term/index.html
//! [state]:   rc/state/index.html
//!
//! [parse]: pre/parse/index.html
//! [scope]: rc/scope/index.html
//!
//! [pre]: pre/index.html
//! [rc]:   rc/index.html
//! [arc]: arc/index.html

extern crate alloc;
extern crate lazy_st;
extern crate nom;
#[macro_use]
extern crate log;

pub mod pre;
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

pub mod error;
mod fmt;
mod stack;

pub use error::Error;
