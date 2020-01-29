use super::*;

type Bound = Vec<String>;

pub fn bind<X, A, F>(bnd: &mut Vec<X>, arg: Option<X>, f: F) -> A
where
    F: FnOnce(&mut Vec<X>) -> A,
{
    match arg {
        Some(id) => {
            bnd.push(id);
            let x = f(bnd);
            bnd.pop();
            x
        }
        None => f(bnd),
    }
}

impl Term {
    pub fn scope(self, sig: &Signature, bnd: &mut Bound) -> Result<Self, Error> {
        use super::Term::*;
        match self {
            Kind => Ok(Kind),
            Type => Ok(Type),
            Symb(s) => match bnd.iter().rev().position(|id| *id == s) {
                Some(idx) => Ok(BVar(idx)),
                None => {
                    if sig.contains_symbol(&s) {
                        Ok(Symb(s))
                    } else {
                        Err(Error::UndeclaredSymbol)
                    }
                }
            },
            Appl(head, tail) => {
                let tail: Result<_, _> =
                    tail.into_iter().map(|tm| Ok(tm.scope(sig, bnd)?)).collect();
                Ok(Appl(Box::new(head.scope(sig, bnd)?), tail?))
            }
            Abst(arg, tm) => {
                let arg = arg.scope(sig, bnd)?;
                bind(bnd, arg.id.clone(), |bnd| {
                    Ok(Abst(arg, Box::new(tm.scope(sig, bnd)?)))
                })
            }
            Prod(arg, tm) => {
                let arg = arg.scope(sig, bnd)?;
                bind(bnd, arg.id.clone(), |bnd| {
                    Ok(Prod(arg, Box::new(tm.scope(sig, bnd)?)))
                })
            }
            BVar(_) => panic!("found bound variable during scoping"),
        }
    }
}

impl Arg {
    fn scope(self, sig: &Signature, bnd: &mut Bound) -> Result<Arg, Error> {
        let ty = self
            .ty
            .map(|ty| Ok(Box::new(ty.scope(sig, bnd)?)))
            .transpose()?;
        Ok(Arg { id: self.id, ty })
    }
}

impl DCommand {
    pub fn scope(self, sig: &Signature, bnd: &mut Bound) -> Result<Self, Error> {
        self.map_type_err(|tm| Ok(Box::new(tm.scope(sig, bnd)?)))?
            .map_term_err(|tm| Ok(Box::new(tm.scope(sig, bnd)?)))
    }
}

#[derive(Debug)]
pub enum Error {
    UndeclaredSymbol,
    MillerPattern,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "scoping error")
    }
}

impl Pattern {
    pub fn scope(self, sig: &Signature, mvar: &Bound, bvar: &mut Bound) -> Result<Self, Error> {
        let scope_many = |args: Vec<Pattern>, bvar: &mut Bound| -> Result<Vec<_>, _> {
            args.into_iter().map(|a| a.scope(sig, mvar, bvar)).collect()
        };
        match self {
            Pattern::Symb(s, args) => match bvar.iter().rev().position(|id| *id == s) {
                Some(idx) => Ok(Pattern::BVar(idx, scope_many(args, bvar)?)),
                None => match mvar.iter().position(|id| *id == s) {
                    Some(idx) => {
                        let args: Option<Vec<_>> =
                            args.into_iter().map(|a| a.get_de_bruijn()).collect();
                        let args = args.ok_or(Error::MillerPattern)?;
                        Ok(Pattern::MVar(Miller(idx), args))
                    }
                    None => {
                        if sig.contains_symbol(&s) {
                            Ok(Pattern::Symb(s, scope_many(args, bvar)?))
                        } else {
                            Err(Error::UndeclaredSymbol)
                        }
                    }
                },
            },
            Pattern::Abst(arg, pat) => bind(bvar, arg.clone(), |bvar| {
                Ok(Pattern::Abst(arg, Box::new(pat.scope(sig, mvar, bvar)?)))
            }),
            _ => unimplemented!(),
        }
    }
}

impl Rule {
    fn scope(mut self, sig: &Signature) -> Result<Self, Error> {
        Ok(Rule {
            ctx: self.ctx.clone(),
            pat: self.pat.scope(sig, &self.ctx, &mut Vec::new())?,
            rhs: self.rhs.scope(sig, &mut self.ctx)?,
        })
    }
}
