//! Unscoped signature-changing commands.

use super::preterm::{BPreterm, Prearg};
use super::Prerule;
use alloc::{boxed::Box, string::String, vec::Vec};

/// Unscoped signature-changing command.
///
/// In contrast to its scoped `Command` counterpart,
/// `Precommand`s hold (pre-)arguments for definitions/declarations.
/// For example, in the definition
/// `f (x : A) : B := t`
/// the argument for `f` is `(x : A)`, and
/// the whole definition is interpreted as
/// `f : ! x : A -> B := \ x : A => t`.
///
/// TODO: make a test here?
#[derive(Clone, Debug)]
pub enum Precommand {
    /// Introduce a new name
    Intro(String, Vec<Prearg>, PreIntroType),
    /// Add a rewrite rule
    Rule(Prerule),
}

pub type PreIntroType = GIntroType<BPreterm, BPreterm>;

/// The way we introduce a new name.
#[derive(Debug, Clone)]
pub enum GIntroType<Ty, Tm> {
    Definition(Option<Ty>, Option<Tm>),
    Theorem(Ty, Tm),
    Declaration(Ty),
}

impl<Ty, Tm> GIntroType<Ty, Tm> {
    pub fn map_type<F, U>(self, f: F) -> GIntroType<U, Tm>
    where
        F: FnOnce(Ty) -> U,
    {
        match self {
            Self::Definition(ty, tm) => GIntroType::Definition(ty.map(f), tm),
            Self::Theorem(ty, tm) => GIntroType::Theorem(f(ty), tm),
            Self::Declaration(ty) => GIntroType::Declaration(f(ty)),
        }
    }
    pub fn map_term<F, U>(self, f: F) -> GIntroType<Ty, U>
    where
        F: FnOnce(Tm) -> U,
    {
        match self {
            Self::Definition(ty, tm) => GIntroType::Definition(ty, tm.map(f)),
            Self::Theorem(ty, tm) => GIntroType::Theorem(ty, f(tm)),
            Self::Declaration(ty) => GIntroType::Declaration(ty),
        }
    }

    pub fn map_type_err<F, U, E>(self, f: F) -> Result<GIntroType<U, Tm>, E>
    where
        F: FnOnce(Ty) -> Result<U, E>,
    {
        match self {
            Self::Definition(ty, tm) => Ok(GIntroType::Definition(ty.map(f).transpose()?, tm)),
            Self::Theorem(ty, tm) => Ok(GIntroType::Theorem(f(ty)?, tm)),
            Self::Declaration(ty) => Ok(GIntroType::Declaration(f(ty)?)),
        }
    }

    pub fn map_term_err<F, U, E>(self, f: F) -> Result<GIntroType<Ty, U>, E>
    where
        F: FnOnce(Tm) -> Result<U, E>,
    {
        match self {
            Self::Definition(ty, tm) => Ok(GIntroType::Definition(ty, tm.map(f).transpose()?)),
            Self::Theorem(ty, tm) => Ok(GIntroType::Theorem(ty, f(tm)?)),
            Self::Declaration(ty) => Ok(GIntroType::Declaration(ty)),
        }
    }
}

impl PreIntroType {
    pub fn parametrise(self, args: Vec<Prearg>) -> Self {
        self.map_type(|tm| Box::new(tm.prods(args.clone())))
            .map_term(|tm| Box::new(tm.absts(args)))
    }
}
