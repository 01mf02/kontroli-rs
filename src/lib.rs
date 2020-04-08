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
//! A command is parsed from a string to yield a [Precommand].
//! The scoping operation then refines the precommand to a [Command],
//! verifying whether the names referenced in the precommand
//! have been previously declared in the [Symbols] table.
//! Once we have a command, we distinguish whether it
//! introduces a name or adds a rewrite rule:
//! In case of a rewrite rule, we add the rewrite rule to the signature.
//! In case of a name introduction, we first
//! update the [Symbols] table with the newly introduced name and
//! verify that the given types and terms are valid by typing them,
//! yielding a signature [Entry].
//! Once we have an entry, we add it to the signature.
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
//! # use kontroli::rc::{Command, Signature, Symbols, Typing};
//! let cmds = [
//!     // declarations
//!     "prop : Type.",
//!     "imp : :prop -> :prop -> prop.",
//!
//!     // definition with a rewrite rule
//!     "def proof : :prop -> Type.",
//!     "[x, y] proof (imp x y) --> :proof x -> proof y.",
//!
//!     // theorem
//!     r"thm imp_refl (x : prop) : proof (imp x x) := \ p : proof x => p.",
//! ];
//!
//! let mut syms = Symbols::new();
//! let mut sig = Signature::new();
//!
//! for c in cmds.iter() {
//!     // parse to Precommand and scope to Command in one go
//!     let cmd: Command = Command::parse(c, &syms)?;
//!     match cmd {
//!         // introduction of a new name
//!         Command::Intro(id, it) => {
//!             // add symbol to symbol table and fail if it is not new
//!             let sym = syms.insert(id)?;
//!
//!             // typecheck and insert into signature
//!             let typing: Typing = Typing::new(it, &sig)?.check(&sig)?;
//!             sig.insert(&sym, typing)?
//!         }
//!         // addition of a rewrite rule
//!         Command::Rule(rule) => sig.add_rule(rule)?,
//!     }
//! }
//! # Ok::<_, Error>(())
//! ~~~
//!
//! # Organisation
//!
//! This library is organised around *data structures* and *functions*.
//! Some modules, such as [pattern], [term], and [state]
//! revolve around data structures of the same name and
//! define only basic functions proper to them.
//! Other modules, such as [parse], [scope], and [typing]
//! define functions that are common to multiple data structures.
//!
//! For many shared data structures, we have a corresponding prestructure;
//! for example, precommands for commands, preterms for terms, and so on.
//! These prestructures are
//! constructed by the parser and
//! refined into their corresponding shared structures by `scope` functions.
//! Prestructures also, unlike their corresponding shared structures,
//! implement the `Send` and `Sync` traits, meaning that
//! they can be transferred and shared between threads.
//! This allows parsing and checking to be performed in parallel.
//!
//! [Kontroli]: https://github.com/01mf02/kontroli-rs
//!
//! [Symbols]: symbols/struct.Symbols.html
//! [Signature]: signature/struct.Signature.html
//! [Precommand]: precommand/enum.Precommand.html
//! [Command]: command/enum.Command.html
//! [Entry]: signature/struct.Entry.html
//!
//! [pattern]: pattern/index.html
//! [term]: term/index.html
//! [state]: state/index.html
//!
//! [parse]: parse/index.html
//! [scope]: scope/index.html
//! [typing]: typing/index.html

extern crate alloc;
extern crate lazy_st;
extern crate nom;
#[macro_use]
extern crate log;

pub mod arc {
    use alloc::sync::Arc as Rc;
    include!("kernel/mod.rs");
}
pub mod rc {
    use alloc::rc::Rc;
    include!("kernel/mod.rs");
}

pub mod error;
pub mod pre;
mod stack;

pub use error::Error;
