//! Unscoped signature-changing commands.

use super::term::Arg;
use super::{Intro, Rule};
use alloc::{string::String, vec::Vec};

/// Unscoped signature-changing command.
///
/// In contrast to its scoped `Command` counterpart,
/// `pre::Command`s hold (pre-)arguments for definitions/declarations.
/// For example, in the definition
/// `f (x : A) : B := t`
/// the argument for `f` is `(x : A)`, and
/// the whole definition is interpreted as
/// `f : ! x : A -> B := \ x : A => t`.
///
/// TODO: make a test here?
#[derive(Clone, Debug)]
pub enum Command {
    /// Introduce a new name
    Intro(String, Vec<Arg>, Intro),
    /// Add a rewrite rule
    Rule(Rule),
}
