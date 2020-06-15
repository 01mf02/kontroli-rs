mod convertible;
#[path = "../scope/intro.rs"]
mod intro;
mod matching;
#[path = "../scope/pattern.rs"]
pub mod pattern;
mod reduce;
pub mod rterm;
#[path = "../scope/rule.rs"]
mod rule;
mod share;
mod signature;
pub mod state;
mod subst;
#[path = "../scope/term.rs"]
pub mod term;
pub mod typing;

use super::Rc;

pub use intro::Intro;
pub use pattern::Pattern;
pub use rterm::RTerm;
pub use rule::Rule;
pub use signature::Signature;
pub use term::Term;
pub use typing::Typing;

use crate::scope::Symbol;
