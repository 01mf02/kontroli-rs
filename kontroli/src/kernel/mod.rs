mod convertible;
mod infer_check;
mod reduce;
mod sterm;
mod subst;
pub mod typing;

use crate::lterm::LTerm;
use crate::share::{Intro, Pattern, Rule, TopPattern};

pub type GCtx<'s> = crate::GCtx<crate::Symbol<'s>, Pattern<'s>, LTerm<'s>>;
