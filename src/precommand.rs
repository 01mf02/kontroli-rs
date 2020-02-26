use crate::preterm::{BPreterm, Prearg};

#[derive(Debug, Clone)]
pub enum Precommand {
    DCmd(String, Vec<Prearg>, PreDCommand),
    Rule(Vec<String>, BPreterm, BPreterm),
}

pub type PreDCommand = GDCommand<BPreterm, BPreterm>;

#[derive(Debug, Clone)]
pub enum GDCommand<Ty, Tm> {
    Definition(Option<Ty>, Option<Tm>),
    Theorem(Ty, Tm),
    Declaration(Ty),
}

impl<Ty, Tm> GDCommand<Ty, Tm> {
    pub fn map_type<F, U>(self, f: F) -> GDCommand<U, Tm>
    where
        F: FnOnce(Ty) -> U,
    {
        match self {
            Self::Definition(ty, tm) => GDCommand::Definition(ty.map(f), tm),
            Self::Theorem(ty, tm) => GDCommand::Theorem(f(ty), tm),
            Self::Declaration(ty) => GDCommand::Declaration(f(ty)),
        }
    }
    pub fn map_term<F, U>(self, f: F) -> GDCommand<Ty, U>
    where
        F: FnOnce(Tm) -> U,
    {
        match self {
            Self::Definition(ty, tm) => GDCommand::Definition(ty, tm.map(f)),
            Self::Theorem(ty, tm) => GDCommand::Theorem(ty, f(tm)),
            Self::Declaration(ty) => GDCommand::Declaration(ty),
        }
    }

    pub fn map_type_err<F, U, E>(self, f: F) -> Result<GDCommand<U, Tm>, E>
    where
        F: FnOnce(Ty) -> Result<U, E>,
    {
        match self {
            Self::Definition(ty, tm) => Ok(GDCommand::Definition(ty.map(f).transpose()?, tm)),
            Self::Theorem(ty, tm) => Ok(GDCommand::Theorem(f(ty)?, tm)),
            Self::Declaration(ty) => Ok(GDCommand::Declaration(f(ty)?)),
        }
    }

    pub fn map_term_err<F, U, E>(self, f: F) -> Result<GDCommand<Ty, U>, E>
    where
        F: FnOnce(Tm) -> Result<U, E>,
    {
        match self {
            Self::Definition(ty, tm) => Ok(GDCommand::Definition(ty, tm.map(f).transpose()?)),
            Self::Theorem(ty, tm) => Ok(GDCommand::Theorem(ty, f(tm)?)),
            Self::Declaration(ty) => Ok(GDCommand::Declaration(ty)),
        }
    }
}

impl PreDCommand {
    pub fn parametrise(self, args: Vec<Prearg>) -> Self {
        self.map_type(|tm| Box::new(tm.prods(args.clone())))
            .map_term(|tm| Box::new(tm.absts(args)))
    }
}
