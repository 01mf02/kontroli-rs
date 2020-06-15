use super::RTerm;
use crate::pre::intro::GIntro;

/// The way we introduce a new name.
pub type Intro<'s> = GIntro<RTerm<'s>, RTerm<'s>>;
