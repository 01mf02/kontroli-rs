//! Substitution and shifting for terms.

use super::{RTerm, Term, TermC};
use alloc::vec::Vec;

impl<'s> RTerm<'s> {
    pub fn apply_subst<S>(self, subst: &S, k: usize) -> Self
    where
        S: Fn(usize, usize) -> Term<'s>,
    {
        let sub = |tm: Term<'s>| tm.apply_subst(subst, k);
        match &*self {
            TermC::Appl(f, args) => {
                let f2 = sub(f.clone());
                let args2: Vec<Term> = args.iter().cloned().map(sub).collect();
                if !f.ptr_eq(&f2) || !args.iter().zip(args2.iter()).all(|(a, a2)| a.ptr_eq(a2)) {
                    return TermC::Appl(f2, args2).into();
                }
            }
            TermC::Abst(arg, f) => {
                let arg2 = arg.clone().map_type(|o| o.map(sub));
                let f2 = f.clone().apply_subst(subst, k + 1);
                if !arg.type_ptr_eq(&arg2) || !f.ptr_eq(&f2) {
                    return TermC::Abst(arg2, f2).into();
                }
            }
            TermC::Prod(arg, f) => {
                let arg2 = arg.clone().map_type(sub);
                let f2 = f.clone().apply_subst(subst, k + 1);
                if !arg.ty.ptr_eq(&arg2.ty) || !f.ptr_eq(&f2) {
                    return TermC::Prod(arg2, f2).into();
                }
            }
        };
        self
    }
}

impl<'s> Term<'s> {
    pub fn apply_subst<S>(self, subst: &S, k: usize) -> Self
    where
        S: Fn(usize, usize) -> Term<'s>,
    {
        match self {
            Self::BVar(n) if n >= k => subst(n, k),
            Self::Comb(c) => Self::Comb(c.apply_subst(subst, k)),
            _ => self,
        }
    }

    pub fn subst(self, u: &Term<'s>) -> Self {
        self.apply_subst(&u.psubst_single(), 0)
    }

    fn psubst_single<'t>(&'t self) -> impl Fn(usize, usize) -> Term<'s> + 't {
        move |n: usize, k: usize| {
            if n == k {
                self.clone() << k
            } else {
                Term::BVar(n - 1)
            }
        }
    }
}

/// Definition of `<<` for terms.
#[allow(clippy::suspicious_arithmetic_impl)]
impl<'s> core::ops::Shl<usize> for Term<'s> {
    type Output = Self;

    fn shl(self, rhs: usize) -> Self::Output {
        if rhs == 0 {
            self
        } else {
            self.apply_subst(&|n, _k| Term::BVar(n + rhs), 0)
        }
    }
}
