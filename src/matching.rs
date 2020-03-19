use crate::pattern::MillerCtx;
use crate::term::{Arg, RTerm, Term};

impl RTerm {
    fn add_n_lambdas(self, n: usize) -> RTerm {
        std::iter::repeat(Arg { id: None, ty: None })
            .take(n)
            .fold(self, |acc, arg| RTerm::new(Term::Abst(arg, acc)))
    }

    fn solve(self, k: usize, var: &MillerCtx) -> Result<RTerm, ()> {
        match &*self {
            Term::Type | Term::Kind | Term::Symb(_) => Ok(self),
            Term::BVar(n) => {
                if *n < k {
                    // var bound in term
                    Ok(self)
                } else {
                    let n2 = match var.arg_pos.get(*n - k) {
                        // var free in term
                        None => n - var.depth + var.args.len(),
                        // variable that is not in the arguments
                        Some(None) => return Err(()),
                        // variable that is in the arguments
                        Some(Some(n2)) => n2 + k,
                    };
                    Ok(RTerm::new(Term::BVar(n2)))
                }
            }
            Term::Abst(Arg { id, ty }, tm) => {
                let ty = ty.clone().map(|ty| Ok(ty.solve(k, var)?)).transpose()?;
                let tm = tm.clone().solve(k + 1, var)?;
                Ok(RTerm::new(Term::Abst(Arg { id: id.clone(), ty }, tm)))
            }
            Term::Prod(Arg { id, ty }, tm) => {
                let ty = ty.clone().map(|ty| Ok(ty.solve(k, var)?)).transpose()?;
                let tm = tm.clone().solve(k + 1, var)?;
                Ok(RTerm::new(Term::Prod(Arg { id: id.clone(), ty }, tm)))
            }
            Term::Appl(f, args) => {
                let f = f.clone().solve(k, var)?;
                let args: Result<_, _> = args
                    .iter()
                    .cloned()
                    .map(|arg| Ok(arg.solve(k, var)?))
                    .collect();
                Ok(RTerm::new(Term::Appl(f, args?)))
            }
        }
    }

    pub fn solve_real(self, var: &MillerCtx) -> Result<RTerm, ()> {
        // TODO: optimisation when args.is_empty()?
        Ok(self.solve(0, var)?.add_n_lambdas(var.args.len()))
    }
}
