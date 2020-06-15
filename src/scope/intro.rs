use super::RTerm;
use crate::pre::intro::GIntroType;

/// The way we introduce a new name.
pub type IntroType<'s> = GIntroType<RTerm<'s>, RTerm<'s>>;
