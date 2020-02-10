use crate::pattern::{Arity, Pattern};
use crate::symbol::Symbol;
use crate::term::{Arg, Term};

pub struct Rule {
    // TODO: save ctx as Stack?
    pub ctx: Vec<(String, Arity)>,
    pub symbol: Symbol,
    pub args: Vec<Pattern>,
    pub rhs: Term,
}

impl std::fmt::Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let pat = Pattern::Symb(self.symbol.clone(), self.args.clone());
        write!(f, "{} ⟶ {}", &pat, self.rhs)
    }
}

#[derive(Debug)]
pub enum Error {
    AVariableIsNotAPattern,
    MillerPattern,
    NonLinearPattern,
    MillerUnused,
    NotEnoughArguments,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

impl Rule {
    pub fn new(ctx: Vec<String>, pat: Pattern, rhs: Term) -> Result<Self, Error> {
        let ctx = pat.arities(ctx)?;
        let (symbol, args) = pat.get_symb_appl().ok_or(Error::AVariableIsNotAPattern)?;
        if !rhs.check_arity(0, &ctx) {
            return Err(Error::NotEnoughArguments);
        }
        Ok(Rule {
            ctx,
            symbol,
            args,
            rhs,
        })
    }
}
