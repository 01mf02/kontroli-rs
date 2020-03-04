//! Rewrite rules.

use crate::pattern::{Arity, Pattern, TopPattern, NoTopPattern};
use crate::prerule::GRule;
use crate::term::{Arg, RTerm, Term};
use std::convert::TryFrom;
use std::fmt;

pub type UncheckedRule = GRule<String, Pattern, RTerm>;
pub type Rule = GRule<(String, Arity), TopPattern, RTerm>;

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pat = Pattern::from(self.lhs.clone());
        write!(f, "{} ⟶ {}", &pat, self.rhs)
    }
}

#[derive(Debug)]
pub enum Error {
    AVariableIsNotAPattern,
    MillerPattern,
    MillerUnused,
    NotEnoughArguments,
    NonLinearNonEqArguments,
}

impl From<NoTopPattern> for Error {
    fn from(_: NoTopPattern) -> Self {
        Self::AVariableIsNotAPattern
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "rule error")
    }
}

// TODO: name type of k "Lambdas"?

impl Arg {
    fn check_arity(&self, k: usize, arities: &[(String, Arity)]) -> bool {
        self.ty.as_ref().map_or(true, |t| t.check_arity(k, arities))
    }
}

impl Term {
    fn check_arity(&self, k: usize, arities: &[(String, Arity)]) -> bool {
        match self {
            Self::Kind | Self::Type | Self::BVar(_) | Self::Symb(_) => true,
            Self::Appl(head, args) => {
                (match **head {
                    Self::BVar(n) if n >= k => args.len() >= arities.get(n - k).expect("arity").1,
                    _ => true,
                }) && args.iter().all(|a| a.check_arity(k, arities))
            }
            Self::Abst(arg, tm) | Self::Prod(arg, tm) => {
                arg.check_arity(k, arities) && tm.check_arity(k + 1, arities)
            }
        }
    }
}

impl TryFrom<UncheckedRule> for Rule {
    type Error = Error;

    fn try_from(unchecked: UncheckedRule) -> Result<Self, Error> {
        let ctx = unchecked.lhs.arities(unchecked.ctx)?;
        let lhs = TopPattern::try_from(unchecked.lhs)?;
        if !unchecked.rhs.check_arity(0, &ctx) {
            return Err(Error::NotEnoughArguments);
        }
        let rhs = unchecked.rhs;
        Ok(Rule { ctx, lhs, rhs })
    }
}
