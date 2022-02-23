use core::fmt::{self, Display};

/// The way we introduce a new name.
#[derive(Debug, Clone)]
pub enum Intro<Ty, Tm = Ty> {
    Definition(Option<Ty>, Option<Tm>),
    Theorem(Ty, Tm),
    Declaration(Ty),
}

impl<Ty, Tm> Intro<Ty, Tm> {
    pub fn rewritable(&self) -> bool {
        match self {
            Self::Definition(..) => true,
            Self::Declaration(_) | Self::Theorem(..) => false,
        }
    }

    pub fn map_type<U>(self, f: impl FnOnce(Ty) -> U) -> Intro<U, Tm> {
        match self {
            Self::Definition(ty, tm) => Intro::Definition(ty.map(f), tm),
            Self::Theorem(ty, tm) => Intro::Theorem(f(ty), tm),
            Self::Declaration(ty) => Intro::Declaration(f(ty)),
        }
    }

    pub fn map_term<U>(self, f: impl FnOnce(Tm) -> U) -> Intro<Ty, U> {
        match self {
            Self::Definition(ty, tm) => Intro::Definition(ty, tm.map(f)),
            Self::Theorem(ty, tm) => Intro::Theorem(ty, f(tm)),
            Self::Declaration(ty) => Intro::Declaration(ty),
        }
    }
}

impl<Ty: Display, Tm: Display> Display for Intro<Ty, Tm> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Definition(Some(ty), Some(tm)) | Self::Theorem(ty, tm) => {
                write!(f, ": {} := {}", ty, tm)
            }
            Self::Definition(Some(ty), None) | Self::Declaration(ty) => write!(f, ": {}", ty),
            Self::Definition(None, tm) => tm.iter().try_for_each(|tm| write!(f, ":= {}", tm)),
        }
    }
}
