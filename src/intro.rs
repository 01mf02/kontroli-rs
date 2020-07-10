/// The way we introduce a new name.
#[derive(Debug, Clone)]
pub enum GIntro<Ty, Tm> {
    Definition(Option<Ty>, Option<Tm>),
    Theorem(Ty, Tm),
    Declaration(Ty),
}

impl<Ty, Tm> GIntro<Ty, Tm> {
    pub fn map_type<F, U>(self, f: F) -> GIntro<U, Tm>
    where
        F: FnOnce(Ty) -> U,
    {
        match self {
            Self::Definition(ty, tm) => GIntro::Definition(ty.map(f), tm),
            Self::Theorem(ty, tm) => GIntro::Theorem(f(ty), tm),
            Self::Declaration(ty) => GIntro::Declaration(f(ty)),
        }
    }
    pub fn map_term<F, U>(self, f: F) -> GIntro<Ty, U>
    where
        F: FnOnce(Tm) -> U,
    {
        match self {
            Self::Definition(ty, tm) => GIntro::Definition(ty, tm.map(f)),
            Self::Theorem(ty, tm) => GIntro::Theorem(ty, f(tm)),
            Self::Declaration(ty) => GIntro::Declaration(ty),
        }
    }

    pub fn map_type_err<F, U, E>(self, f: F) -> Result<GIntro<U, Tm>, E>
    where
        F: FnOnce(Ty) -> Result<U, E>,
    {
        match self {
            Self::Definition(ty, tm) => Ok(GIntro::Definition(ty.map(f).transpose()?, tm)),
            Self::Theorem(ty, tm) => Ok(GIntro::Theorem(f(ty)?, tm)),
            Self::Declaration(ty) => Ok(GIntro::Declaration(f(ty)?)),
        }
    }

    pub fn map_term_err<F, U, E>(self, f: F) -> Result<GIntro<Ty, U>, E>
    where
        F: FnOnce(Tm) -> Result<U, E>,
    {
        match self {
            Self::Definition(ty, tm) => Ok(GIntro::Definition(ty, tm.map(f).transpose()?)),
            Self::Theorem(ty, tm) => Ok(GIntro::Theorem(ty, f(tm)?)),
            Self::Declaration(ty) => Ok(GIntro::Declaration(ty)),
        }
    }
}
