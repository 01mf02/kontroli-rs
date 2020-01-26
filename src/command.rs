use crate::term::*;

#[derive(Debug, Clone)]
pub enum Command {
    DCmd(String, Vec<Arg>, DCommand),
    Rule(Vec<String>, BTerm, BTerm),
}

#[derive(Debug, Clone)]
pub enum DCommand {
    Definition(Option<BTerm>, Option<BTerm>),
    Theorem(BTerm, BTerm),
    Declaration(BTerm),
}

impl DCommand {
    pub fn map_type<F>(self, f: F) -> DCommand
    where
        F: FnOnce(BTerm) -> BTerm,
    {
        match self {
            Self::Definition(ty, tm) => Self::Definition(ty.map(f), tm),
            Self::Theorem(ty, tm) => Self::Theorem(f(ty), tm),
            Self::Declaration(ty) => Self::Declaration(f(ty)),
        }
    }
    pub fn map_term<F>(self, f: F) -> DCommand
    where
        F: FnOnce(BTerm) -> BTerm,
    {
        match self {
            Self::Definition(ty, tm) => Self::Definition(ty, tm.map(f)),
            Self::Theorem(ty, tm) => Self::Theorem(ty, f(tm)),
            Self::Declaration(ty) => Self::Declaration(ty),
        }
    }

    pub fn map_type_err<F, E>(self, f: F) -> Result<DCommand, E>
    where
        F: FnOnce(BTerm) -> Result<BTerm, E>,
    {
        match self {
            Self::Definition(ty, tm) => Ok(Self::Definition(ty.map(f).transpose()?, tm)),
            Self::Theorem(ty, tm) => Ok(Self::Theorem(f(ty)?, tm)),
            Self::Declaration(ty) => Ok(Self::Declaration(f(ty)?)),
        }
    }

    pub fn map_term_err<F, E>(self, f: F) -> Result<DCommand, E>
    where
        F: FnOnce(BTerm) -> Result<BTerm, E>,
    {
        match self {
            Self::Definition(ty, tm) => Ok(Self::Definition(ty, tm.map(f).transpose()?)),
            Self::Theorem(ty, tm) => Ok(Self::Theorem(ty, f(tm)?)),
            Self::Declaration(ty) => Ok(Self::Declaration(ty)),
        }
    }

    pub fn parametrise(self, args: Vec<Arg>) -> DCommand {
        self.map_type(|tm| Box::new(tm.prods(args.clone())))
            .map_term(|tm| Box::new(tm.absts(args)))
    }
}
