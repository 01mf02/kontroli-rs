use core::fmt::{self, Display};

/// The way we introduce a new name.
#[derive(Debug, Clone)]
pub enum Intro<Ty, Tm> {
    Definition(Option<Ty>, Option<Tm>),
    Theorem(Ty, Tm),
    Declaration(Ty),
}

impl<Ty, Tm> Intro<Ty, Tm> {
    pub fn map_type<F, U>(self, f: F) -> Intro<U, Tm>
    where
        F: FnOnce(Ty) -> U,
    {
        match self {
            Self::Definition(ty, tm) => Intro::Definition(ty.map(f), tm),
            Self::Theorem(ty, tm) => Intro::Theorem(f(ty), tm),
            Self::Declaration(ty) => Intro::Declaration(f(ty)),
        }
    }
    pub fn map_term<F, U>(self, f: F) -> Intro<Ty, U>
    where
        F: FnOnce(Tm) -> U,
    {
        match self {
            Self::Definition(ty, tm) => Intro::Definition(ty, tm.map(f)),
            Self::Theorem(ty, tm) => Intro::Theorem(ty, f(tm)),
            Self::Declaration(ty) => Intro::Declaration(ty),
        }
    }

    pub fn map_type_err<F, U, E>(self, f: F) -> Result<Intro<U, Tm>, E>
    where
        F: FnOnce(Ty) -> Result<U, E>,
    {
        match self {
            Self::Definition(ty, tm) => Ok(Intro::Definition(ty.map(f).transpose()?, tm)),
            Self::Theorem(ty, tm) => Ok(Intro::Theorem(f(ty)?, tm)),
            Self::Declaration(ty) => Ok(Intro::Declaration(f(ty)?)),
        }
    }

    pub fn map_term_err<F, U, E>(self, f: F) -> Result<Intro<Ty, U>, E>
    where
        F: FnOnce(Tm) -> Result<U, E>,
    {
        match self {
            Self::Definition(ty, tm) => Ok(Intro::Definition(ty, tm.map(f).transpose()?)),
            Self::Theorem(ty, tm) => Ok(Intro::Theorem(ty, f(tm)?)),
            Self::Declaration(ty) => Ok(Intro::Declaration(ty)),
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
